module MP5a where

import Data.Maybe
import Data.Ord
import Data.List
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Map (Map, empty, fromList, (!), keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Monad.State
import System.IO
import System.Console.ANSI
import GHC.IO


-- Search function from lecture notes
search :: (Eq a, Show a) =>
          (a -> Bool)
          -> (a -> [a])
          -> ([a] -> [a] -> [a])
          -> [a] -> [a]
          -> Maybe a
search goal adj comb unvisited visited
  | null unvisited = Nothing
  | goal (head unvisited) = Just (head unvisited)
  | otherwise = let (n:ns) = unvisited
                in -- debug n $ -- uncomment to "debug"
                   search goal adj comb
                          (comb (removeDups (adj n)) ns)
                          (n:visited)
  where removeDups = filter (not . (`elem` (unvisited ++ visited)))


debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 1*10^5) `seq`
            y


-- Call with an admissible heuristic as the cost function to carry out A* search
bestFirstSearch :: (Eq a, Show a, Ord b) =>
                   (a -> Bool)
                   -> (a -> [a])
                   -> (a -> b)
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = search goal succ comb [start] []
  where comb new old = (sortOn cost new) ++ old

dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = search goal succ (++) [start] []

adjLocs :: Board -> [KLoc]
adjLocs b@(Board (w,h) p@((x,y):_) cmap) = 
  [(x', y') | (dx, dy) <- [(-1,-2), (1,-2), (2,-1), (2,1),(1,2),(-1,2),(-2,1),(-2,-1)],
              let (x', y') = (x+dx, y+dy),
              x' > 0 && x' <= w,
              y' > 0 && y' <= h]

type BoardDims = (Int, Int) -- (width,height)

type KLoc = (Int, Int) -- (x,y); (1,1) @ top-left

type KPath = [KLoc] -- path through board

data Board = Board { 
              boardDims :: BoardDims, 
              kPath :: KPath,
              boardAdjMap :: Map KLoc [KLoc] 
            } deriving (Eq)

drawWallsNW :: KLoc -> Board -> (String, String)
drawWallsNW c@(x, y) (Board (w, h) p cmap) = 
  let nc = (x, y-1)
      wc = (x-1, y)
      adj = findWithDefault [] c cmap
  in ("+" ++ if nc `elem` adj then "   " else "---",
      (if wc `elem` adj then " " else "|") 
        ++ (if c `elem` p then " o " else "   "))

drawBoard :: Board -> String
drawBoard m@(Board (w, h) _ _) = (concat $ map drawRow $ chunksOf w drawnCells) 
                                 ++ bot
  where drawRow cs = let (l1, l2) = unzip cs
                     in concat l1 ++ "+\n" ++ concat l2 ++ "|\n"
        drawnCells = [drawWallsNW (x, y) m | y <- [1..h], x <- [1..w]]
        bot = (concat $ replicate w "+---") ++ "+"

instance Show Board where
  show = drawBoard      

solveBoard :: (Ord a) => (Board -> a) -> Board -> Maybe Board
solveBoard cost b@(Board (w,h) p _) =
  bestFirstSearch ((== (w*h)) . length . kPath)  
         nextPaths
         cost
         (b { kPath = p})

knightsTour :: Board -> Maybe Board
knightsTour b@(Board dims p@(loc:_) cmap)= solveBoard cost b
              where cost b2 = length (filter (not . flip elem p) $ adjLocs b2)

showPath :: Maybe Board -> [KLoc]
showPath (Just b) = reverse $ getPath b
showPath Nothing = []

getPath :: Board -> KPath
getPath (Board _ p _) = p
                            
nextPaths :: Board -> [Board]
nextPaths b@(Board dims p@(loc:_) _) = do
  adj <- filter (not . flip elem p) $ adjLocs b 
  return $ b { kPath = adj:p }

-- Supply a starting a position as such
-- showPath $ knightsTour (Board (5,5) [(3,3)] empty)

-- Despite using Warnsdorff's rule as a heuristic 
-- Supplying the beginning path of a known solution
-- (such as the one in the gif in the assigment) helps finding a particular solution

-- showPath $ knightsTour (Board (5,5) [(1,2),(3,1),(5,2),(3,3)] empty)
-- showPath $ knightsTour (Board (5,5) (reverse [(3,3),(5,2),(3,1),(1,2),(2,4),(4,5),(5,3)]) empty)
