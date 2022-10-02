module MP3b  where

import Data.List
import Data.List.Split ( chunksOf )
import Test.QuickCheck


{-
  Playing card definitions (feel free to add your own supporting types, so long 
  as you keep `Card`).
-}
data Card = Card Suit Int deriving (Show)
data Suit = Clubs | Hearts | Spades | Diamonds deriving (Show)

instance Eq Card where
  (Card s1 a) == (Card s2 b) = a == b

instance Ord Card where
  (Card s1 a) `compare` (Card s2 b) = a `compare` b

instance Eq Suit where
  Clubs == Clubs = True 
  Hearts == Hearts = True 
  Spades == Spades = True 
  Diamonds == Diamonds = True 

  Clubs == Hearts = False 
  Clubs == Spades = False 
  Clubs == Diamonds = False 

  Hearts == Clubs = False
  Hearts == Spades = False
  Hearts == Diamonds = False

  Spades == Clubs = False
  Spades == Hearts = False
  Spades == Diamonds = False

  Diamonds == Clubs = False
  Diamonds == Hearts = False
  Diamonds == Spades = False

{-
  A full deck of 52 playing cards.
-}
deck :: [Card]
deck = genRanks 1 Clubs ++ genRanks 1 Hearts ++ genRanks 1 Spades ++ genRanks 1 Diamonds 

genRanks :: Int -> Suit -> [Card]
genRanks n suit | n < 13 = Card suit n: genRanks (n+1) suit
                | otherwise = [Card suit n]


{-
  Hand types. Don't change these.
-}
data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight
            | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Show, Ord)



{-
  Takes a list of 5 cards and returns the strongest hand they can be
  used to make. 

  Examples (note that your `Card` values may look different):

  hand [Card 2 H, Card 3 D, Card Ace H, Card 5 D, Card 4 S]
  => Straight

  hand [Card 2 D, Card 3 C, Card 2 C, Card 3 D, Card 2 H]
  => FullHouse
-}
hand :: [Card] -> Hand
hand cards = unpack $ take 1 (hands cards)
unpack [a] = a
-- nice thing about 'hands' is that it shows all available hands 
hands c = (filter ( /= HighCard) [royalFlush c, straightFlush c, fourOfAKind c, fullHouse c, flush c, straight c, threeOfAKind c, twoPair c, onePair c] )++ [HighCard]


royalFlush cards =  if (straightFlush cards == StraightFlush) && (royalCount 0 cards == 4) then RoyalFlush
                        else HighCard
straightFlush cards = if (fiveSequence sorted || fiveSequence ace) && sameSuit sorted then StraightFlush
                        else HighCard
                    where sorted = sort cards
                          ace = aceCheck sorted 


fourOfAKind cards = if 4 `elem` occurrences cards cards then FourOfAKind
                        else HighCard
threeOfAKind cards = if 3 `elem` occurrences cards cards then ThreeOfAKind
                        else HighCard

fullHouse cards = if (3 `elem` occurrences cards cards) && (2 `elem` occurrences cards cards) then FullHouse
                        else HighCard

flush cards = if sameSuit cards && not (fiveSequence sorted || fiveSequence ace) then Flush
                        else HighCard
                    where sorted = sort cards
                          ace = aceCheck sorted
straight cards = if not (sameSuit cards) && (fiveSequence sorted || fiveSequence ace) then Straight
                        else HighCard
                    where sorted = sort cards
                          ace = aceCheck sorted


twoPair cards = if pairs 2 (occurrences cards cards) 0 == 4 then TwoPair  
                        else HighCard
                    
onePair cards = if pairs 2 (occurrences cards cards) 0 >= 2 then Pair
                        else HighCard


-- ////////////////////////////////////////////////////////////////////////////////////////
aceCheck :: [Card] -> [Card]
aceCheck cards = drop 1 cards ++ take 1 cards

fiveSequence :: [Card] -> Bool
fiveSequence [Card s1 13 , Card s2 1] = fiveSequence [Card s2 1]
fiveSequence [Card s a] = True
fiveSequence ((Card s1 a):(Card s2 b):cards) | a+1 == b  = fiveSequence ( Card s2 b :cards)
                                             | otherwise = False
sameSuit :: [Card] -> Bool
sameSuit [Card s a] = True
sameSuit ((Card s1 a):(Card s2 b):cards) | s1 == s2 = sameSuit (Card s2 b:cards)
                                         | otherwise = False
royalCount :: Int -> [Card] -> Int
royalCount n [] = n
royalCount n ((Card s no):cards) = if no > 10 || no == 1 then royalCount (n+1) cards 
                                    else royalCount n cards

rankCount ::  Int -> Int -> [Card] -> Int
rankCount c _ [] = c
rankCount c cardRank ((Card s n):cards) | cardRank == n = rankCount (c+1) cardRank cards
                                        | otherwise = rankCount c cardRank cards
-- occurrences is kind of ineffecient but ill just roll with it
occurrences :: [Card] -> [Card] -> [Int]
occurrences [] cards = []
occurrences (c:ards) cards  = map (\ c -> rankCount 0 (getN c) cards) cards

pairs :: Int -> [Int] -> Int -> Int
pairs _ [] pN = pN
pairs n (x:xs) pN = if x == n then pairs n xs (pN+1)
                      else pairs n xs pN

getN :: Card -> Int
getN (Card _ n) = n
-- ///////////////////////////////////////////////////////////////////////////////////////////////////////


{-
  Takes a list of 5-`Card` lists, and returns a list of tuples of type 
  `(Int, Hand)`, where each tuple indicates the number of times a certain 
  `Hand` occurs in the input list. The tuples should be sorted in decreasing 
  order of frequency.
  
  See the machine problem write-up on how to test this function with the 
  generators defined for you below.
-}
computeStats :: [[Card]] -> [(Int, Hand)]
computeStats cs =  filter ((/=0).fst) (tupifyHands $ countHands 0 0 0 0 0 0 0 0 0 0 $ getHands cs) 

getHands cs = [hand c | c <- cs]
countHands ::  Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Hand] -> [Int]
countHands rF sF fourK fH f s threeK tP p hC [] = [rF, sF, fourK, fH, f, s, threeK, tP, p, hC]
countHands rF sF fourK fH f s threeK tP p hC (h:hands) | h == RoyalFlush = countHands (rF+1) sF fourK fH f s threeK tP p hC hands
                                                       | h == StraightFlush = countHands rF (sF+1) fourK fH f s threeK tP p hC hands
                                                       | h == FourOfAKind = countHands rF sF (fourK+1) fH f s threeK tP p hC hands
                                                       | h == FullHouse = countHands rF sF fourK (fH+1) f s threeK tP p hC hands
                                                       | h == Flush = countHands rF sF fourK fH (f+1) s threeK tP p hC hands
                                                       | h == Straight = countHands rF sF fourK fH f (s+1) threeK tP p hC hands
                                                       | h == ThreeOfAKind = countHands rF sF fourK fH f s (threeK+1) tP p hC hands
                                                       | h == TwoPair = countHands rF sF fourK fH f s threeK (tP+1) p hC hands
                                                       | h == Pair = countHands rF sF fourK fH f s threeK tP (p+1) hC hands
                                                       | h == HighCard = countHands rF sF fourK fH f s threeK tP p (hC+1) hands
tupifyHands [rF, sF, fourK, fH, f, s, threeK, tP, p, hC] = reverse $ sort [(rF,RoyalFlush), (sF, StraightFlush), (fourK,FourOfAKind), (fH,FullHouse),
                                                             (f,Flush), (s,Straight), (threeK,ThreeOfAKind), (tP,TwoPair), (p,Pair), (hC,HighCard)]


-------------------------------------------------------------------------------

{- 
  Random deck/hand generators -- you shouldn't change any of the following
  functions!
-}

genDeck :: Gen [Card]
genDeck = shuffle deck

genHand :: Gen [Card]
genHand = (take 5) <$> genDeck

genHands :: Int -> Gen [[Card]]
genHands n = (take n . chunksOf 5) <$> genDeck

test :: Int -> IO [(Int, Hand)]
test n = generate $  computeStats <$> (vectorOf n genHand)
