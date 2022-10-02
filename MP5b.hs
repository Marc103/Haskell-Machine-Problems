{-# LANGUAGE FlexibleInstances #-}
module MP5b where

import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split
import Data.Tree
import Data.Map (Map, empty, fromList, (!), findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO


{- Replace with your own game data types  -}

data Piece = O | X deriving (Eq, Show, Read)
opponent X = O
opponent O = X

type BoardC4 = [[Maybe Piece]] 

instance {-# OVERLAPPING #-} Show BoardC4 where
  show = topbottom .intercalate "\n" . chunksOf 23 . str . strsArrs . reverse
    where showSquare Nothing = "   "
          showSquare (Just p) = " " ++ (show p) ++ " "
          topbottom str ="\n  1  2  3  4  5  6  7 \n" ++ str ++ "\n^^^^^^^^^^^^^^^^^^^^^^^"
          str c4 = concat (map concatBars c4)
          strsArrs c4 = map (map showSquare) c4
          concatBars ls = "|" ++ concat ls ++ "|"
          

emptyBoard :: BoardC4
emptyBoard = replicate 6 (replicate 7 (Nothing))
fullBoard = replicate 6 (replicate 7 (Just X))

-- Orientation of board is upside down like:
-- | O X O       X X X   |
-- | O X                 |
-- | O                   |
-- |                     |
-- |                     |
-- |                     |
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- easier to treat the list as already reversed, edit, then reverse again when showing 
-- default row starts at 1 (treat col input as col-1)
-- illegal moves can be identified by checking if the state of the board didnt change
playMove :: Piece -> Int -> Bool -> BoardC4 -> BoardC4
playMove _ _ _ [] = []
playMove p col done (r:bc4) | not done = sMove : playMove p col d bc4      
                            | otherwise = r: playMove p col done bc4
                            where tempR = r 
                                  sMove = setMove p (col-1) r 0 
                                  d = (tempR /= sMove)
        

setMove :: Piece -> Int -> [Maybe Piece] -> Int -> [Maybe Piece]
setMove _ _ [] _ = []
setMove p col (i:row) counter = if col == counter && isNothing i then (Just p) : setMove p col row (counter+1)  
                                         else i:setMove p col row (counter+1) 
playMoveClean p col bc4 = playMove p col False bc4

playMoves moves = play moves X emptyBoard
  where play [] _ bc4 = bc4
        play (m:ms) p bc4 = play ms (opponent p) $ playMoveClean p m bc4 


turnC4 :: [[Maybe Piece]] -> Piece
turnC4 bc4 = let (xs,os) = tally $ map (turnR) bc4
                 in if xs == os then X else O

turnR :: [Maybe Piece] -> (Int, Int)
turnR brow = let xs = length $ filter (== Just X) brow
                 os = length $ filter (== Just O) brow
                 in (xs,os)

tally [] = (0,0)
tally (xo:xos) = xo `addTuple` tally xos

addTuple (x,y) (a,b) = (x+a, y+b)

-- Checking for win
-- work with normally orientated board (so reverse it)
-- |                     |
-- | O                   |
-- | O  X  O             |
-- | X  O  X             |
-- | O  X  O             |
-- | X  O  X  X          |
-- ^^^^^^^^^^^^^^^^^^^^^^^
-- for each column prob the depth to locate row
-- point is i only have to check the stuff at the surface 

-- reversing in function, don't need to reverse when calling 
-- start at col 0
probe :: Int -> Int -> BoardC4 -> (Int, Int)
probe col 6 bc4 = (-1,-1) -- column is empty, nothing to check 
probe col row bc4 = if isJust (getItem col (getItem row (reverse bc4))) then (col,row)
                       else probe col (row+1) bc4
probeClean col = probe col 0 
getItem n ls = ls !! n 

-- now with locations, need to do diagonal and horizontal and vertical checks
locations :: BoardC4 -> [(Int, Int)]
locations bc4 = filter (/= (-1,-1)) [probe col 0 bc4 | col <- [0..6]]

-- before that let me define next moves (similar construct to locations), in this case no need to reverse (easier that way)
probeRev col 6 bc4 = (-1,-1) -- column is full, no availabe move 
probeRev col row bc4 = if isNothing (getItem col (getItem row bc4)) then (col,5-row)
                       else probeRev col (row+1) bc4
nextMoves bc4 = filter (/= (-1,-1)) [probeRev col 0 bc4 | col <- [0..6]]

probeRevPlus col 6 bc4 = (col,-1) -- act at location of ceiling (which will work with hiddenCombinedChecks)
probeRevPlus col row bc4 = if isNothing (getItem col (getItem row bc4)) then (col,5-row)
                            else probeRevPlus col (row+1) bc4
nextMovesPlus bc4 = [probeRevPlus col 0 bc4 | col <- [0..6]]

-- from these locations i want to check the longest chain of p
-- and determine if a win occured 
-- later the length of the chain will be used as my scoring function 
-- using 'diverge' functions
-- make sure ajust pos so that its not overlapping 

center :: Piece -> BoardC4 -> (Int, Int) -> Bool
center p bc4 (x,y) = (getItem x (getItem y bc4)) == (Just p) 
                        
divergeTL :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeTL p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeTL p bc4 (x-1,y-1) (add counter) break
                                   | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

divergeTR :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeTR p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeTR p bc4 (x+1,y-1) (add counter) break
                                   | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p) 

divergeBL :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeBL p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeBL p bc4 (x-1,y+1) (add counter) break
                                   | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p) 

divergeBR :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeBR p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeBR p bc4 (x+1,y+1) (add counter) break
                                   | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

divergeR  :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeR p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeR p bc4 (x+1,y) (add counter) break
                                  | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

divergeL  :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeL p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeL p bc4 (x-1,y) (add counter) break
                                  | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

divergeU  :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeU p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeU p bc4 (x,y-1) (add counter) break
                                  | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

divergeD  :: Piece -> BoardC4 -> (Int, Int) -> Int -> Bool -> Int
divergeD p bc4 (x,y) counter flag | (x >= 0 && x <= 6) && (y >= 0 && y <= 5) && flag = divergeD p bc4 (x,y+1) (add counter) break
                                  | otherwise = counter   
                                    where add counter = if break then (counter+1)
                                                           else counter
                                          break = (getItem x (getItem y bc4)) == (Just p)

-- (x >= 0 && x <= 6) && (y >= 0 && y <= 5)
-- i just realized how ineffecient calling reverse on every recursive call of diverge_ is 
-- oh well
-- ok fine ill just call it manually 

combineChecks p bc4 (x,y) = if center p bc4 (x,y) then [1 + (divergeTL p bc4 (x-1,y-1) 0 True) + (divergeBR p bc4 (x+1,y+1) 0 True),
                                                        1 + (divergeTR p bc4 (x+1,y-1) 0 True) + (divergeBL p bc4 (x-1,y+1) 0 True),
                                                        1 + (divergeL p bc4 (x-1,y) 0 True) + (divergeR p bc4 (x+1,y) 0 True),
                                                        1 + (divergeU p bc4 (x,y-1) 0 True) + (divergeD p bc4 (x,y+1) 0 True)]
                            else []
-- REMEBER TO CALL WITH REVERSE on bc4


win p bc4 (x,y) = 4 `elem` (combineChecks p (reverse bc4) (x,y)) ||
                  5 `elem` (combineChecks p (reverse bc4) (x,y)) ||
                  6 `elem` (combineChecks p (reverse bc4) (x,y)) ||
                  7 `elem` (combineChecks p (reverse bc4) (x,y)) 
-- small bug its possible to get a line longer than 4

wins p bc4 = True `elem` map (win p bc4) (locations bc4)




full :: BoardC4 -> Bool
full bc4 = all (==True) [all (/= Nothing) r | r <- bc4] 
              
playInteractive :: IO ()
playInteractive = play X emptyBoard 
  where play turn board
          | wins X board = putStrLn "X wins!"
          | wins O board = putStrLn "O wins!"
          | full board = putStrLn "Drawn"
          | otherwise = do
              putStr "Enter a move: "
              move <- readLn
              if (playMoveClean turn move board) == board -- detect illegal move by comparing prev to curr state of board
                then do putStrLn "Illegal move"
                        play turn board
                else do let board' = playMoveClean turn move board
                        print board'
                        play (opponent turn) board'


{- Some convenience functions and types -- feel free to replace/modify  -}

gameTree :: BoardC4 -> Tree BoardC4
gameTree bc4 = Node bc4 
                $ map gameTree 
                $ map (\(x,_) -> playMoveClean (turnC4 bc4) (x+1) bc4) (tillTerminate bc4)

-- Major issue with how the game tree is created
-- the problem is that in its current state, it will throw a game just to achieve a higher net score
-- because the tree continues despite if someone won or lost (and so the subsequent values after the game
-- has been won keep getting add in the net score calculation)
-- luckily the fix to fix my sacrificing AI was simple, add the win condition so that if it is met,
-- an empty list of potential next moves is passed (instead of just nextMoves)

tillTerminate bc4 = if ((wins X bc4) || (wins O bc4)) then [] else nextMoves bc4

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ns) = Node x $ map (prune (n-1)) ns

data Scored a = Scored { score :: Int, scoredVal :: a }


instance Eq (Scored a) where
  (Scored x _) == (Scored y _) = x == y


instance Ord (Scored a) where
  compare (Scored x _) (Scored y _) = compare x y


instance Show a => Show (Scored a) where
  show (Scored s v) = "Score: " ++ show s ++ "\n\n" ++ show v


-- basically rank by longest line
-- The search algorithm is sufficient to determine a win, but for scoring I need to do additional checks
-- for example it won't be able to tell that X is about to win in this scenario 
--  1  2  3  4  5  6  7
-- |                     |
-- |                     |
-- |                     |
-- |                     |
-- | O  O  O             |
-- | X  X  X             |
-- ^^^^^^^^^^^^^^^^^^^^^^^
-- now this is a bit more complicated than I initially thought it would be because there are 7 cases to take care of 7 cases
-- luckily all I have to do is modify combineChecks (remove vertical check aswell in hiddenc checks)
-- and the locations will be based on nextMoves
--
--  # # ?    ? # #     X #       # ? #
--  # X #    # X #     # X       # X #  
--  X # #    # # X     # # ?     # X #
--   BL        BR       TL         D
--    # X    # #         # #
--    X #    X X ?     ? X X
--  ? # #    # # #     # # #
--    TR       L         R

--     # # #     
--     # X #    
--     X # #    
--   ? # # #
-- Only issue is that the AI will react to a situation like this one and think that it is
-- a good idea to put X at the ? (when really its pointless) but its a definite improvement AND
-- more importantly it will identify this pattern if its Os and try to block it
-- Just realized that combineHiddenChecks can be used with a modified nextMoves
-- this will also take care of the surplus tie situations where the next possible boards do not affect
-- the longest chain (with how I implemented my checks initially)

combineHiddenChecks :: Piece -> BoardC4 -> (Int, Int) -> [Int]
combineHiddenChecks p bc4 (x,y) = [(divergeTL p bc4 (x-1,y-1) 0 True) +
                                   (divergeBR p bc4 (x+1,y+1) 0 True),
                                   (divergeTR p bc4 (x+1,y-1) 0 True) + 
                                   (divergeBL p bc4 (x-1,y+1) 0 True),
                                   (divergeL p bc4 (x-1,y) 0 True) + 
                                   (divergeR p bc4 (x+1,y) 0 True),
                                   (divergeD p bc4 (x,y+1) 0 True)]
                                  

findMax p bc4 =  filter (/= []) $ map (combineHiddenChecks p (reverse bc4)) (nextMovesPlus bc4)
findMaxCheck max = if max == [] then 0
                      else maximum $ map maximum max
-- also found a glaring issue with max, i was doing maximum $ maximum when really
-- i needed to map first then find max

-- also need to adjust the tie scores 
scoreBoard :: Piece -> BoardC4 -> Scored BoardC4
scoreBoard p bc4 | wins p bc4 = Scored 100 bc4
                 | wins (opponent p) bc4 = Scored (-100) bc4
                 | user >= opp = Scored (25*user) bc4
                 | opp > user = Scored (-25*opp) bc4 
                 where user = findMaxCheck $ findMax p bc4  
                       opp = findMaxCheck $ findMax (opponent p) bc4

printScoredTree :: BoardC4 -> IO ()
printScoredTree = putStrLn . drawTree . fmap (show . scoreBoard X) . (prune 1) . gameTree


-- Minimax function from lecture notes
minimax :: (a -> Scored a) -> Tree a -> Scored a
minimax scorefn (Node _ ns) = maximize ns
  where maximize = maximumBy (comparing score) . map (eval minimize)
        minimize = minimumBy (comparing score) . map (eval maximize)
        eval _ (Node x []) = scorefn x
        eval f (Node x ns) = let Scored s _ = f ns in Scored s x


-- Plays a game with the AI
playAI :: IO ()
playAI = play X emptyBoard
  where play _ b | wins X b = putStrLn "X wins!"
                 | wins O b = putStrLn "O wins!"
                 | full b = putStrLn "Draw"
        play X b = do 
          putStr "Enter a move: "
          move <- readLn
          if (playMoveClean X move b) == b
              then do putStrLn "Illegal move"
                      play X b
              else do let b' = playMoveClean X move b
                      print b'
                      play O b'
        play O b = do
          let b' = scoredVal $ minimax (scoreBoard O) (prune 4 (gameTree b))
          print b'
          play X b'