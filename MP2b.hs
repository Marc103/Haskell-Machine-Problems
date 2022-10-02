module MP2b where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Makes a barren world for the Game of Life. A world is a 2-tuple of:
--  1. its width (W) and height (H), as a tuple of integers
--  2. a list of H lists of W Bools, each value representing a cell's state
--     (True for living, False for dead)
makeWorld :: (Int,Int)  -- (width,height) of world
          -> ((Int,Int), [[Bool]])  -- world
makeWorld dims@(w,h) = (dims, replicate h $ replicate w False)


-- Computes the number of living neighbors of a cell.
liveNeighbors :: ((Int,Int), [[Bool]])  -- world
              -> (Int,Int)  -- cell
              -> Int  -- num of living neighbors
liveNeighbors a@((w,h), cells) b@(x,y) = checkAbove a b + check a b + checkBelow a b 

checkAbove ((w,h), cells) (x,y) = if (y+1) < h then intBool cellLeft + intBool cellMid + intBool cellRight
                                  else 0
                                    where rowAbove = iterRows (y+1) cells
                                          cellLeft = iterCell (x-1) rowAbove w
                                          cellMid = iterCell x rowAbove w
                                          cellRight = iterCell (x+1) rowAbove w

checkBelow ((w,h), cells) (x,y) = if (y-1) >= 0 then intBool cellLeft + intBool cellMid + intBool cellRight
                                  else 0
                                    where rowBelow = iterRows (y-1) cells
                                          cellLeft = iterCell (x-1) rowBelow w
                                          cellMid = iterCell x rowBelow w
                                          cellRight = iterCell (x+1) rowBelow w



check ((w,h), cells) (x,y) = intBool cellLeft + intBool cellRight
                             where row = iterRows y cells
                                   cellLeft = iterCell (x-1) row w
                                   cellRight = iterCell (x+1) row w
intBool bool = if bool then 1 else 0                          
iterRows y [] = []
iterRows 0 (row:cells) = row
iterRows y (row:cells) = iterRows (y-1) cells
                          
iterCell 0 (cell:row) w = cell
iterCell x (cell:row) w = if x >= 0 && x < w then iterCell (x-1) row w
                            else False

-- Computes the next world state according to Conrad's rules:
--  1. Birth: a dead cell with exactly three living neighbors comes alive.
--  2. Survival: a living cell with two to three neighbors remains alive. 
--  3. Death: a living cell with zero or one neighbors dies in isolation; 
--            a living cell with four or more neighbors dies of overcrowding
nextWorld :: ((Int,Int), [[Bool]])  -- current world
          -> ((Int,Int), [[Bool]])  -- next world
nextWorld a@((width,height), cells) = ((width,height),batch a (0,0))


--batch a@((width,height), (r:cells)) (x,y) 0  = [r]
batch :: ((Int, Int), [[Bool]]) -> (Int, Int) -> [[Bool]]
batch a@((width,height), cells) (x,y)  = if y /= height then (futureCells a (x,y) (iterRows y cells)): (batch a (x,y+1)) else [iterRows y cells]

-- start at 0,0 for x,y and each time the row from cell needs to be worked on using futureCells (which then rules are applied to)


futureCells :: ((Int, Int), [[Bool]]) -> (Int, Int) -> [Bool] -> [Bool]
futureCells a@((width,height), cells) (x,y) [] = []
futureCells a@((width,height), cells) (x,y) (b:bools) = if x /= width then (rules a (x,y) b):futureCells a (x+1,y) bools else [(rules a (x,y) b)]

rules a@((width,height), cells) (x,y) b | b == False && living == 3 = True 
                                | b == True && (living >= 2 && living <= 3) = True 
                                | otherwise = False
                                    where living = liveNeighbors a (x,y)




-- Draw a picture of the world
drawWorld :: ((Int,Int), [[Bool]])  -- world
          -> Picture
drawWorld a@((w,h) , cells) = Pictures (combinePicsToList (iteratePictures a (0,0)))

combinePicsToList [] = []
combinePicsToList (x:xs) = x ++ combinePicsToList xs

-- Similar to batch, future functions (probably someway i could have used higher order functions here)
iteratePictures :: ((Int, Int), [[Bool]]) -> (Int, Int) -> [[Picture]]
iteratePictures a@((width,height), cells) (x,y) = if y < height then (procIn a (x,y) (iterRows y cells)): iteratePictures a (x,y+1) else [(procIn a (x,y) (iterRows y cells))]
procIn a@((width,height), cells) (x,y) [] = []
procIn a@((width,height), cells) (x,y) (b:bools)  = if x < width then (picTrans width height (x, y) b): procIn a (x+1,y) bools else [picTrans width height (x, y) b]                                              

-- Below is the scaling stuff
picTrans :: Int -> Int -> (Int, Int) -> Bool -> Picture
picTrans width height (x,y) bool = if bool then translate fx fy sqAlive
                      else translate fx fy sqDead
                            where 
                                  fx =  (intToFloat x) * (scaleW) - 250 + centerW
                                  fy =  (intToFloat y) * (scaleH) - 250 + centerH
                                  sqAlive = color blue  (rectangleSolid scaleW scaleH)
                                  sqDead = color orange (rectangleSolid scaleW scaleH)
                                  scaleW = 500 / intToFloat width
                                  scaleH = 500 / intToFloat height
                                  centerW = scaleW / 2
                                  centerH = scaleH / 2

intToFloat :: Int -> Float
intToFloat x = fromIntegral x/1.0


-- Handle an event affecting the world. The only event we handle is a mouse
-- click, which will create life in the targeted cell.
handleEvents :: Event  -- event information
             -> ((Int,Int), [[Bool]])  -- world
             -> ((Int,Int), [[Bool]])
handleEvents (EventKey (MouseButton LeftButton) Up _ (mx,my)) 
             world@((w,h), cells) 
    = ((w,h), giveLife world (0,0) (convCoor (mx, my) (w,h)))

handleEvents _ world = world

--Convert mouse coordinates to grid coordinate, need to subtract ~0.5 so that when rounded the entire block will be considered 
convCoor :: (Float, Float) -> (Int, Int) -> (Int, Int) 
convCoor (x, y) (width, height) = (floatToInt ix, floatToInt iy)
            where scaleW = 500 / intToFloat width
                  scaleH = 500 / intToFloat height
                  ix = ((x + 250) / scaleW) - 0.47
                  iy = ((y + 250) / scaleH) - 0.47
floatToInt x = round x

--Finally need to change targeted cell (here we go again...)
giveLife a@((width,height), cells) (x,y) (tx, ty)  = if y < height then (life a (x,y) (tx, ty) (iterRows y cells)): (giveLife a (x,y+1) (tx, ty)) else [(life a (x,y) (tx, ty) (iterRows y cells))]
life a@((width,height), cells) (x,y) (tx, ty) [] = []
life a@((width,height), cells) (x,y) (tx, ty) (b:bools) = if x /= width then (target (x,y) (tx, ty) b):life a (x+1,y) (tx, ty) bools else [(target (x,y) (tx, ty) b)]
target (x,y) (tx, ty) b = if (x == tx) && (y == ty) then True else b






