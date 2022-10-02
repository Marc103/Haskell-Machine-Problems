module MP1 where

import Data.Char
import Graphics.Gloss

-- Part 1: Polymorphic functions from types

p1_1 :: a -> b -> b
p1_1 x y = y


p1_2 :: (a -> b -> c) -> (a,b) -> c
p1_2 f (x,y) = f x y

p1_3 :: (a -> b) -> (b -> c) -> a -> c
p1_3 f g m =  g (f m)


p1_4 :: (a -> b -> c) -> a -> (d -> b) -> d -> c
p1_4 f1 f2 f3 f4 = f1 f2 (f3 f4)


-- Part 2: Function implementations 


-- 1. Transposes a 2-row x 2-column tuple.
--
--    e.g., transposeTup ((1,2),(3,4)) = ((1,3),(2,4))
transposeTup :: ((a,a),(a,a))  -- input matrix
             -> ((a,a),(a,a))  -- transposed matrix
transposeTup ((x1, y1),(x2, y2)) = ((x1,x2),(y1,y2))


-- 2. Sorts the elements of a 3-tuple.
--
--    e.g., sort3Tup (2,1,3) = (1,2,3)
--          sort3Tup (3,2,1) = (1,2,3)
sort3Tup :: Ord a 
         => (a,a,a)  -- input 3-tuple
         -> (a,a,a)  -- sorted 3-tuple
sort3Tup (x,y,z) | x > y = sort3Tup (y,x,z)
                 | y > z = sort3Tup (x,z,y)
                 | otherwise = (x,y,z)


-- 3. Computes the compound interest earned.
--    e.g., compoundInterest 100 0.2 1 = 20
--          compoundInterest 100 0.2 2 = 44
compoundInterest :: Floating a 
                 => a   -- principal
                 -> a   -- rate
                 -> Int -- num of compounding periods
                 -> a   -- amount of compound interest earned

compoundInterest principle rate period  = (principle*((1+rate)^period)) - principle 


-- 4. Computes the length of the Collatz sequence starting at the input.
--
--    e.g., collatzLen 10 = 7
--          collatzLen 27 = 112
collatzLen :: Integer  -- start value of the sequence
           -> Integer  -- length of sequence
collatzLen n = count n 1

count :: Integer -> Integer -> Integer
count n m = if n == 1 then m
            else count (collatz n) (m+1)
collatz n | n == 1 = 1
          | even n = n `div` 2
          | otherwise = (3*n)+1


-- 5. Computes the square root of the input using Newton's method.
--
--    e.g., newtonsSqrt 2 ~= 1.4142...
--          newtonsSqrt 1000 ~= 31.6227...
newtonsSqrt :: (Floating a, Ord a) 
            => a -- x
            -> a -- square root of x
newtonsSqrt a = root a 1 

root a guess
    | goodEnough = guess
    | otherwise = root a improve
    where
        goodEnough = abs (guess^2 - a) < 0.0001
        improve = (guess + a / guess) / 2

-- 6. Draws a planet in a circular orbit given an orbital radius and period.
drawOrbit :: Float  -- radius
          -> Float  -- period
          -> Float  -- time
          -> Picture
drawOrbit r p t = translate (r*cos((2*pi*t)/p)) (r*sin((2*pi*t)/p)) (circleSolid 10)


-- 7. Draws a planet in an elliptical orbit based on Kepler's equation.
drawOrbit' :: Float  -- semi-major axis
           -> Float  -- eccentricity
           -> Float  -- period
           -> Float  -- time
           -> Picture
drawOrbit' a e p t = translate (r*cos theta) (r*sin theta) (circleSolid 10)
            where 
                n = (2*pi)/p
                m = n*t
                ea = solveEA e m 1
                theta = 2* atan(sqrt((1+e)/(1-e)) * tan(ea/2))
                r = a*(1-(e * cos ea))

solveEA e m ea
    | goodEnough = ea
    | otherwise = solveEA e m improve 
    where
        goodEnough = abs (m - (ea - e*sin ea)) < 0.0001
        improve = (ea + (m + e * sin ea))/2