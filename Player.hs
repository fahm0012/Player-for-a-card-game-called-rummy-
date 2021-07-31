-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Parser.Instances
import Data.Char

-- pickCard checks two conditions one is memory and the other is picking the card from Discard or Stock
-- When Memory is Nothing then new Memory return is empty if memory is not Empty then old memory is send
-- Memory is main controlled by playCard
-- Picks card from Discard if it forms a meld else pick card from Stock
pickCard :: ActionFunc
pickCard c _ Nothing _ playerHand 
  | callCheckStraight c playerHand || checkSet c playerHand =  (Discard,"")
  | otherwise = (Stock,"")
pickCard c _ (Just m) _ playerHand 
  | callCheckStraight c playerHand || checkSet c playerHand =  (Discard,m)
  | otherwise = (Stock,m)

-- Sorts hand 
quicksort :: [Card] -> [Card]  
quicksort [] = []  
quicksort (x:xs) =   
    let lessthanpivot = quicksort [a | a <- xs, a <= x] 
        biggerthanpivot = quicksort [a | a <- xs, a > x ]
    in  lessthanpivot ++ [x] ++ biggerthanpivot

--Sorts hand according to rank
quicksortbyRank :: [Card] -> [Card]  
quicksortbyRank [] = []  
quicksortbyRank (x:xs) =   
    let lessthanpivot = quicksortbyRank [a | a <- xs, getRank a <= getRank x] 
        biggerthanpivot = quicksortbyRank [a | a <- xs, getRank a > getRank x ]
    in  lessthanpivot ++ [x] ++ biggerthanpivot

-- Retrieves Suit of the hand
getSuit:: Card -> Suit
getSuit (Card s _ ) = s

-- Retrieves Rank of the hand
getRank:: Card -> Rank
getRank (Card _ r) = r

-- This is used to check if the Card can be added to hand [Card] to form a straight.If it does it returns True.
-- This is a caller function which uses helper functions called checkStraight,quicksort,getSuit in-order to achieve its task.
-- Main thing about this function is that it filters the hand [Card] according to the suit of the Card
callCheckStraight:: Card -> [Card] -> Bool
callCheckStraight c hand  = checkStraight (fromEnum (getRank c)) 
                                              (map (fromEnum . getRank) 
                                                    (quicksort (filter (\x -> getSuit x == getSuit c) hand)))

-- This function is Main Decision Maker for Straights.
-- Int  is Rank of the Card 
-- [Int] is Rank of cards in a hand which belong to the Same Suit as Card
-- Returns True if the Card forms a straight with hand
checkStraight:: Int -> [Int] -> Bool
checkStraight c hand
    | c == 0 = isCardPresent 1 hand && isCardPresent 2 hand
    | c == 12 = isCardPresent 11 hand && isCardPresent 10 hand
    | c == 1 = (isCardPresent 0 hand && isCardPresent 2 hand) || 
               (isCardPresent 2 hand && isCardPresent 3 hand)
    | c == 11 = (isCardPresent 10 hand && isCardPresent 12 hand) || 
                (isCardPresent 10 hand && isCardPresent 9 hand)
    | otherwise = (isCardPresent (c+1) hand && isCardPresent (c-1) hand) ||
                  (isCardPresent (c+1) hand && isCardPresent (c+2) hand) ||
                  (isCardPresent (c-1) hand && isCardPresent (c-2) hand)

-- This function checks If the card is present in the hand.
-- Int is rank of the Card which is being checked if its present in hand
-- [Int] is Rank of cards in a hand which belong to the Same Suit as Card
isCardPresent :: Int -> [Int] -> Bool
isCardPresent _ [] = False
isCardPresent c (x:xs)
  | c == x = True
  | otherwise = isCardPresent c xs

-- This function checks if the Card forms a meld of type Set with hand [Card].
-- In order to achive this it calls helper function called checkSetOtherConditions.
checkSet:: Card -> [Card]-> Bool
checkSet c hand
 | length (filter (\x -> getRank x == getRank c && getSuit x /= getSuit c) hand) < 2 = False
 | otherwise = checkSetOtherConditions 
                          (filter (\x -> getRank x == getRank c && getSuit x /= getSuit c) hand) hand 0

-- Major Conditions checked by this function if the Card forms a meld of type set with the Hand
checkSetOtherConditions:: [Card] -> [Card] -> Int -> Bool
checkSetOtherConditions [] _ len = len  >= 2
checkSetOtherConditions (x:xs) l len = if callCheckStraight x l then
                                            checkSetOtherConditions xs l len
                                       else
                                            checkSetOtherConditions xs l (len+1)

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc 
playCard card s m hand = (playfunc card (quicksort (card:hand)) (quicksort hand) s m, memoryScore s m)

-- This function calls mainAction decider and filter two list which shouldnt contain the card we just picked
-- These first list of cards contain deadwood cards and the second list  contain cards which wont form future melds
-- The other arguments are score,memory and card which we picked.Score changes is used to know if its the first round.
playfunc :: Card -> [Card]->[Card]->(Score,Score)-> String -> Action
playfunc card newhand hand
  = mainActionDecider
      (quicksortbyRank
         (filter (/= card) (formDeadWoodCards newhand newhand)))
      (quicksortbyRank
         (filter (/= card) (cardsWhichCantFormFutureMelds newhand newhand)))
      newhand hand card

-- This function makes decision about Action by looking at the following conditions.
    -- 1) Total deadwood value of hand which is provided as Int
    -- 2) Checking if its not first round by comparing score the present score and score in memory
actionDecider:: Int -> Card->(Score,Score)-> String -> Action
actionDecider value c s m 
  | value == 0 && roundDetection s m = Action Gin c
  | value < 10 && roundDetection s m = Action Knock c
  | otherwise = Action Drop c

-- This function helps us to decide what action we wanna take.
-- It checks if the list of cards which wont form future melds represented by fd isnt empty then remove card from it.
-- If fd is empty then it removes card from list of cards which contain deadwood cards represented by d
-- If fd and d are also empty then it means all cards(hand cards + picked card) are making melds so rearrange melds
mainActionDecider:: [Card]-> [Card]->[Card]-> [Card]->Card->(Score,Score)-> String-> Action
mainActionDecider d fd newhand hand pick s m 
  | not (null fd) = totalMeld (last fd)  newhand s m
  | not (null d) = totalMeld  (last d)  newhand s m
  | otherwise = totalMeld (rearrangeMelds 
                            (formSetCards newhand (notinStraight newhand newhand)++ 
                             helperCardfunc (formStraightCards newhand newhand)) hand pick) newhand s m

-- This function rearranges the melds when all cards (hand cards + picked card) form melds
-- It removes the card from larger melds in order to make space for smaller melds
rearrangeMelds:: [[Card]]-> [Card]-> Card-> Card
rearrangeMelds [] h _= if  not (null (quicksortbyRank (formDeadWoodCards h h))) 
                          then last (quicksortbyRank(formDeadWoodCards h h)) 
                      else  
                          last (quicksortbyRank h)
rearrangeMelds (x : xs) h pc
  | length x >= 4 && pc /= head x = head x
  | length x >= 4 && pc == head x = x !!(length x - 1)
  | otherwise = rearrangeMelds xs h pc

-- This helps to make the Action as it takes the card which we are gonna discard and then it takes the whole hand with card we picked
-- and filters it by removing the card we are gonna throw and count total deadwood value of hand
totalMeld:: Card -> [Card]->(Score,Score)-> String -> Action
totalMeld c newhand
  = actionDecider
      (totalDeadWoodValueOfHand
         (formDeadWoodCards
            (filter (/= c) newhand) (filter (/= c) newhand)))
      c 

-- This form straight melds in card form where each nested list length determines whats the size of the straight meld formed.
helperCardfunc:: [Card] -> [[Card]]
helperCardfunc hand =  formStraightMeldCardForm (filter (\ x -> getSuit x == Spade) hand)
  ++
     formStraightMeldCardForm (filter (\ x -> getSuit x == Club) hand)
      ++
         formStraightMeldCardForm (filter (\ x -> getSuit x == Diamond) hand)
          ++  formStraightMeldCardForm (filter (\ x -> getSuit x == Heart) hand)

-- This function calculates total deadwood value for the hand
totalDeadWoodValueOfHand:: [Card] -> Int
totalDeadWoodValueOfHand [] = 0
totalDeadWoodValueOfHand (x:xs) = deadWoodValueOfCard x + foldr ((+) . deadWoodValueOfCard) 0 xs

-- Tells the deadwood value of each Card
deadWoodValueOfCard:: Card -> Int
deadWoodValueOfCard c 
  | getRank c == King = 10
  | getRank c == Queen = 10
  | getRank c == Jack = 10
  | otherwise = fromEnum(getRank c)+1

-- Memory Function which decides what to pass in memory
memoryScore :: (Score, Score)->String -> String
memoryScore s m 
  | (m == "") || (s /= getMem (parse choices m)) = "(" ++ show (fst s) ++ "," ++ show (snd s) ++ ")"
  | otherwise = m

-- Return True if its First Round else False if some other round
roundDetection:: (Score, Score)->String -> Bool
roundDetection s m 
  | (m == "") || (s /= getMem (parse choices m)) = False
  | otherwise = True


-- Function which takes hand and return cards which are present in the deadwood
formDeadWoodCards:: [Card]->[Card] ->[Card]
formDeadWoodCards [] _ = []
formDeadWoodCards (x:xs) hand = if not (callCheckStraight x hand) && not (checkSet x hand) then
     x:formDeadWoodCards xs hand
else
    formDeadWoodCards xs hand

-- Gives list of cards which cant form melds in Future
cardsWhichCantFormFutureMelds:: [Card]->[Card]->[Card]
cardsWhichCantFormFutureMelds [] _ = []
cardsWhichCantFormFutureMelds (x:xs) hand = if not (callFutureCheckStraight x hand) && 
                                               not (checkFutureSet x hand) 
                                                  then x:cardsWhichCantFormFutureMelds xs hand
                                            else
                                                  cardsWhichCantFormFutureMelds xs hand

-- This use to check if the card on discard can be added to hand to form future straight.If it does it returns True
callFutureCheckStraight:: Card -> [Card] -> Bool
callFutureCheckStraight c hand  = furturecheckStraight (fromEnum (getRank c)) 
                                                          (map (fromEnum . getRank) 
                                                            (quicksortbyRank
                                                              (filter (\x -> getSuit x == getSuit c) hand)))

-- This checks if the card can form straight in future
furturecheckStraight:: Int -> [Int] -> Bool
furturecheckStraight c hand
    |  c == 0 = isCardPresent 1 hand
    |  c == 12 = isCardPresent 11 hand 
    | otherwise = isCardPresent (c+1) hand || isCardPresent (c-1) hand
                  
-- This checks if the Future set can be formed
checkFutureSet:: Card -> [Card]-> Bool
checkFutureSet c hand
 | not(any(\x -> getRank x == getRank c && getSuit x /= getSuit c) hand) = False
 | otherwise = futureCheckSetOtherConditions 
                            (filter (\x -> getRank x == getRank c && getSuit x /= getSuit c) hand) hand 0

-- This check some other conditions that card supplied to form a set is not present in a straight 
-- and it forms a set.If these conitions are true it returns True.
futureCheckSetOtherConditions:: [Card] -> [Card] -> Int -> Bool
futureCheckSetOtherConditions [] _ len = len >= 1
futureCheckSetOtherConditions (x:xs) l len = if callFutureCheckStraight x l then
                                            futureCheckSetOtherConditions xs l len
                                       else
                                            futureCheckSetOtherConditions xs l (len+1)

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ hand = callToMakeStraightMeld (quicksort hand) ++ 
                     callToMakeSetMeld (quicksort hand) ++ 
                     formDeadWood (quicksort hand) (quicksort hand)
   
-- This function is called by makeMelds to form Melds of type Straight
callToMakeStraightMeld:: [Card] -> [Meld]
callToMakeStraightMeld c = helperMeldfunc (formStraightCards (quicksort c) (quicksort c))

-- This function is called by makeMelds to form Melds of type Set
callToMakeSetMeld:: [Card] -> [Meld]
callToMakeSetMeld c = formSetCardMelds (formSetCards c (notinStraight c c))

-- This function filters the hand by removing all cards which are present in straight
notinStraight:: [Card]-> [Card]->[Card]
notinStraight [] _ = []
notinStraight (x:xs) hand = if callCheckStraight x hand then
          notinStraight xs hand
else
  x: notinStraight xs hand
     
-- This Function filters the cards used in the straight
formStraightCards:: [Card]->[Card]->[Card]
formStraightCards [] _ = []
formStraightCards (x:xs) hand = if callCheckStraight x hand then
    x : formStraightCards xs hand
else
    formStraightCards xs hand

-- This filters the card list according to the suits in-order to form straight melds for those suit
-- and then merge them into single meld list
helperMeldfunc:: [Card] -> [Meld]
helperMeldfunc hand = makeStraightMelds (formStraightMeldCardForm (filter (\ x -> getSuit x == Spade) hand))
  ++
    makeStraightMelds (formStraightMeldCardForm (filter (\ x -> getSuit x == Club) hand))
      ++
        makeStraightMelds (formStraightMeldCardForm (filter (\ x -> getSuit x == Diamond) hand))
          ++ makeStraightMelds (formStraightMeldCardForm (filter (\ x -> getSuit x == Heart) hand))

-- This function form the nested card list according to melds formed
formStraightMeldCardForm :: [Card] -> [[Card]]
formStraightMeldCardForm [] = []
formStraightMeldCardForm (x:xs) = (x:formConsectiveCardList x xs) : 
                                      formStraightMeldCardForm (formListofNonConsectiveCards 
                                                                (x:formConsectiveCardList x xs) (x:xs))

-- This function is used to form straights.This function creates list of consective ranks
formConsectiveCardList :: Card -> [Card]-> [Card]
formConsectiveCardList _ [] = []
formConsectiveCardList c (x:xs) = if fromEnum (getRank x)-fromEnum(getRank c) == 1 
                                      then x : formConsectiveCardList x xs 
                                  else 
                                      formConsectiveCardList c xs

-- This function is used to form straights and this function creates a list of cards which form another card list 
-- which werent used in the last straight 
formListofNonConsectiveCards :: [Card]->[Card] -> [Card]
formListofNonConsectiveCards _ [] = []
formListofNonConsectiveCards [] (y:ys) = y:ys
formListofNonConsectiveCards (_:xs) (_:ys) = formListofNonConsectiveCards xs ys

-- This function forms the Melds from the card list which contain the cards which were used in the straights
makeStraightMelds:: [[Card]] -> [Meld]
makeStraightMelds = concatMap decidetypeofStraightMeld

-- This checks whats the form of StraightMeld store in [Card]
decidetypeofStraightMeld:: [Card]-> [Meld]
decidetypeofStraightMeld c
  | length c == 3 =  [Straight3 (head c) (c!!1) (c!!2)]
  | length c == 4 =  [Straight4 (head c) (c!!1) (c!!2) (c!!3)]
  | length c == 5 =  [Straight5 (head c) (c!!1) (c!!2) (c!!3) (c!!4)]
  | length c == 6 =  Straight3 (head c) (c !! 1) (c !! 2) : 
                     [Straight3 (c !! 3) (c !! 4) (c !! 5)]
  | length c == 7 =  Straight3 (head c) (c !! 1) (c !! 2) : 
                     [Straight4 (c !! 3) (c !! 4) (c !! 5) (c !! 6)]
  | length c == 8 =  Straight4 (head c) (c !! 1) (c !! 2) (c !! 3) : 
                     [Straight4 (c !! 4) (c !! 5) (c !! 6) (c !! 7)]
  | length c == 9 =  Straight3 (head c) (c!!1) (c!!2)  : 
                     Straight3 (c !! 3) (c !!4) (c!!5) : 
                     [Straight3 (c !! 6) (c!!7) (c!!8)]
  | length c == 10 = Straight5 (head c) (c!!1) (c!!2) (c!!3) (c!!4) : 
                     [Straight5 (c !! 5) (c!!6) (c!!7) (c!!8) (c!!9)]
  | otherwise = []
  
  
-- This function form nested cards lists where each nested list has a set formed
formSetCards:: [Card]->[Card]->[[Card]]
formSetCards [] _ = []
formSetCards (x:xs) hand = if checkSet x hand then
                              filter (\z -> getRank z == getRank x) hand : 
                              formSetCards (filter (\k -> getRank k /= getRank x) xs) hand
                           else
                              formSetCards xs hand

-- This function form Melds of the nest card list which contain set melds in card form
formSetCardMelds:: [[Card]] -> [Meld]
formSetCardMelds [] = []
formSetCardMelds xs = concatMap decidetypeofSetMeld xs

-- This decides what type of Set Meld is stored in [Card]
decidetypeofSetMeld:: [Card]-> [Meld]
decidetypeofSetMeld c 
  | length c == 3 = [Set3 (head c) (c !! 1) (c !! 2)]
  | length c == 4 = [Set4 (head c) (c !! 1) (c !! 2) (c !! 3)]
  | otherwise = []

-- This function finds out the deadwood cards in the hand
formDeadWood:: [Card]->[Card] -> [Meld]
formDeadWood [] _ = []
formDeadWood (x:xs) hand = if not (callCheckStraight x hand) && not (checkSet x hand) then
    Deadwood x : formDeadWood xs hand
else
    formDeadWood xs hand


-- Parser implementation

-- Using this from Tutorial 11
-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- character
  let next = if f c then pure else unexpectedCharParser
  next c

-- Using this from Tutorial 11
-- checks if it gives a digit
digit :: Parser Char
digit = satisfy isDigit


-- This parses the score tuple of the form (twodigits,twodigits)
scoreParserForTwoDigits:: Parser (Int,Int)
scoreParserForTwoDigits = do
  _ <- is '('
  a <- digit
  b <- digit
  _ <- is ','
  c <- digit
  d <- digit
  _ <- is ')'
  pure (digitToInt a *10+ digitToInt b,digitToInt c *10+ digitToInt d)

--This parses the score tuple of the form (onedigit,onedigit)
scoreParserForOneDigits:: Parser (Int,Int)
scoreParserForOneDigits = do
  _ <- is '('
  a <- digit
  _ <- is ','
  c <- digit
  _ <- is ')'
  pure (digitToInt a,digitToInt c)

--This parses the score tuple of the form (onedigit,twodigit)
scoreParserForOneDigitAndTwoDigit:: Parser (Int,Int)
scoreParserForOneDigitAndTwoDigit = do
  _ <- is '('
  a <- digit
  _ <- is ','
  b <- digit
  c <- digit
  _ <- is ')'
  pure (digitToInt a,digitToInt b *10+ digitToInt c)


-- This parse the score tuple of the form (twodigit,onedigit)
scoreParserForTwoDigitAndOneDigit:: Parser (Int,Int)
scoreParserForTwoDigitAndOneDigit = do
  _ <- is '('
  a <- digit
  b <- digit
  _ <- is ','
  c <- digit
  _ <- is ')'
  pure (digitToInt a*10+digitToInt b,digitToInt c)

-- choose what type of parser to use
choices:: Parser (Int,Int)
choices = scoreParserForTwoDigits ||| 
          scoreParserForOneDigits ||| 
          scoreParserForOneDigitAndTwoDigit ||| 
          scoreParserForTwoDigitAndOneDigit

-- Used it from lecture from lecture notes
-- In order to return the datastructure in parser
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "Score out of Bounds"

