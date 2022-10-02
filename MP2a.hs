module MP2a where
import Data.Char


{-
   Creates N cycles (repetitions) of the input list.


   Examples:

   cycleN 3 [1..4]
   => [1,2,3,4,1,2,3,4,1,2,3,4]

   cycleN 0 "hello?"
   => ""
-}
cycleN :: Int  -- N
       -> [a]  -- input list
       -> [a] 
cycleN n xs | n > 0 = xs ++ (cycleN (n-1) xs)
            | otherwise = xs




{-
   Partitions the input list into sublists of maximum size N.


   Examples:

   chunksOf 3 "hello world"
   => ["hel","lo ","wor","ld"]

   chunksOf 5 [1..3]
   => [[1,2,3]]
-}
chunksOf :: Int  -- N
         -> [a]  -- input list
         -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
            
            

{-
   "Unzips" a list of 4-tuples into a tuple of 4 lists.


   Examples:

   unzip4 [(1,2,3,4),(5,6,7,8),(9,10,11,12),(13,14,15,16)]
   => ([1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16])

   unzip4 [(1,'h',True,3.14), (2,'i',False,2.7), (3,'!',True,9.8)]
   => ([1,2,3],"hi!",[True,False,True],[3.14,2.7,9.8])
-}
unzip4 :: [(a,b,c,d)]  -- list of 4-tuples
       -> ([a], [b], [c], [d])

unzip4 ls = (unzipA ls, unzipB ls, unzipC ls, unzipD ls)

unzipA :: [(a, b, c, d)] -> [a]
unzipA ls = [a | (a,b,c,d) <- ls]

unzipB :: [(a, b, c, d)] -> [b]
unzipB ls = [b | (a,b,c,d) <- ls]

unzipC :: [(a, b, c, d)] -> [c]
unzipC ls = [c | (a,b,c,d) <- ls]

unzipD :: [(a, b, c, d)] -> [d]
unzipD ls = [d | (a,b,c,d) <- ls]

{-
   Concatenates the lists in an input list, with element X interspersed.


   Examples:

   intersperse ',' ["tom","dick","harry"]
   => "tom,dick,harry"

   intersperse '!' ["hi"]
   => "hi"

   intersperse 0 [[1..5],[6..10]]
   => [1,2,3,4,5,0,6,7,8,9,10]
-}
intersperse :: a      -- X
            -> [[a]]  -- input list (of lists)
            -> [a]
intersperse z ls = trim(inter z ls)

inter z [] = []
inter z (x:ls) =  z:x ++ inter z ls
trim (x:ls) = ls
                     


{-
   Removes all values found in the list `candidates` from the input list.


   Examples:

   removeAll [1..3] [0..10]
   => [0,4,5,6,7,8,9,10]

   removeAll "aeiou" "supercalifragilisticexpialidocious"
   => "sprclfrglstcxpldcs"
-}
removeAll :: (Eq a) 
          => [a]  -- candidates list
          -> [a]  -- input list
          -> [a]
removeAll qs [] = []
removeAll qs (x:ls) = if x `elem` qs then removeAll qs ls
                      else x:removeAll qs ls 


{-
   Extracts a sublist of elements in range [M,N) from the input list. 
   Returns a tuple containing the sublist and the input list with the 
   sublist removed.


   Examples:

   sublist (2,7) [0..10]
   => ([2,3,4,5,6],[0,1,7,8,9,10])

   sublist (3,4) [0..10]
   => ([3],[0,1,2,4,5,6,7,8,9,10])

   sublist (5,5) [0..10]
   => ([],[0,1,2,3,4,5,6,7,8,9,10])

   sublist (0,12) "hello world!"
   => ("hello world!","")

   sublist (6,100) "hello world!"
   => ("world!","hello ")
-}
sublist :: (Eq a) => (Int,Int)  -- sublist range (M,N); M is inclusive, N exclusive
        -> [a]        -- input list
        -> ([a],[a])

sublist (a,b) xs = (chop, removeAll chop xs)
                  where chop = cutout (a,b) xs
cutout (a,b) xs = take (b-a) (drop a xs) 


 


{-
   Applies Luhn's algorithm for numeric ID verification:

   The Luhn algorithm is used to verify the validity of numeric identifiers
   commonly used for credit/debit card numbers, government IDs, IMEI numbers,
   etc. Given a list of one-digit numbers, it processes them as follows:
   
   1. From right to left, double the value of every other digit. If a
      product is greater than 9, subtract 9 from that result.
   
   2. Sum up all the digits (i.e., the results from step 1 and the given values
      of the other digits)
   
   3. If the result is evenly divisible by 10, the identifier is valid.
   

   E.g., given the identifier consisting of the numbers [2,7,5,8]:
   
   1. We start by doubling the value of every other number starting from the
      right, getting [4,7,10,8]. Since 10 > 9, we subtract 9 from it,
      giving us the list [4,7,1,8]
   
   2. Sum up all the digits, giving us: 4+7+1+8 = 20
   
   3. 20 is evenly divisible by 10, so the identifier is valid.
   

   E.g., given the identifier consisting of the numbers [4,6,1,8,5,3,8]
   
   1. Doubling every other value and subtracting 9 when needed gets us 
      [4,3,1,7,5,6,8]
   
   2. Summing them gets us 34
   
   3. 34 is not evenly divisible by 10, so the identifier is invalid.
   
   
   Examples:
   
   luhn [2,7,5,8]
   => True
   
   luhn [4,3,1,7,5,6,8]
   => False
   
   luhn [3,9,2,8,6,4,1,7,2,0,5,2]
   => True
-}
luhn :: [Int]  -- numeric ID
     ->  Bool  -- True if valid, False otherwise
luhn ls = if sumLs(iterOdd 0 (reverse ls)) `mod` 10 == 0 then True 
          else False

adj x | 2*x > 9 = 2*x - 9
      | otherwise = 2*x

iterOdd :: Int -> [Int] -> [Int]

iterOdd _ (x:[]) = (adj x):[]
iterOdd a (x:ls) | a `mod` 2 == 0 = x:iterOdd (a+1) (ls)
                 | a `mod` 2 == 1 = (adj x):(iterOdd (a+1) (ls))
                 | otherwise = undefined

sumLs :: [Int] -> Int 

sumLs [] = 0
sumLs (x:xs) = x + sumLs xs 



{-
   Carries out run-length encoding on input string. 
   
   Run-length encoding is a simple form of data compression that replaces
   characters in a stream with the count of adjacent occurrences of that
   character and just a single instance of the character itself. Write a
   function that takes a string and returns a list of tuples reprenting the
   run-length encoding of that string.
   

   Examples:
   
   runLengthEncode "aaaaaaabbb"
   => [(7,'a'),(3,'b')]
   
   runLengthEncode "happy daaay"
   => [(1,'h'),(1,'a'),(2,'p'),(1,'y'),(1,' '),(1,'d'),(3,'a'),(1,'y')]
-}
runLengthEncode :: String -> [(Int,Char)]

runLengthEncode xs = runEn xs []           
runEn xs toAdd = if xs /= [] then (ab):(runEn (drop (getN(ab)) (xs)) toAdd)
                 else toAdd
                  where ab = next 1 xs

-- (drop (getN(next 1 xs)) xs)
getN (n,a) = n

next :: Int  -> String  -> (Int,Char)
next n [] = (0,' ')
next n (a:[]) = (n,a)
next n (a:b:xs) = if a == b then next (n + 1) (b:xs) 
                  else (n, a)





{-
   Decodes the run-length encoding of a string.


   Examples:

   runLengthDecode [(1,'h'), (5,'i')]
   => "hiiiii"
    
   runLengthDecode (runLengthEncode "whhhhaaaaat?")
   => "whhhhaaaaat?"
-}
runLengthDecode :: [(Int,Char)]  -- run-length encoded string
                -> String  -- original string
runLengthDecode xs = compose (runLe xs)
runLe tps = [rep a b |(a,b) <- tps]
compose :: [[a]] -> [a]
compose [] = []
compose (a:xs) = a ++ compose xs
rep n char | n > 0 = char: rep (n-1) char
           | otherwise = []

{- 
   Applies the Vigenere encryption scheme to the input string.

   The Vigenere encryption scheme is similar to the Caesar cipher presented in
   class in that it makes use of shifting, but instead of a single numeric key
   applied uniformly to the entire plain text, a string of characters is used as
   the key. The numeric value of each character (i.e., its position in the
   alphabet) is used as a shift value, and if the key is shorter than the length
   of the plain text it is simply repeated.


   E.g., to encrypt the plain text "FOOBAR" with the key "BAZ", we can proceed
   as follows:

   1. Pair each letter of the plain text with a letter from the key:

         F  O  O  B  A  R B  A  Z  B  A  Z

   2. Convert each letter to its numeric value (A=0, B=1 ... Z=25)

         5  14  14  1  0  17 1   0  25  1  0  25

   3. Add them together:

         6  14  39  2  0  42

   4. "Wrap" the numbers around so they're in the range 0-25:

         6  14  13  2  0  16

   5. Convert the numbers back into letters:

         G  O  N  C  A  Q

   Plain text can contain a mix of lowercase and uppercase letters and
   punctuation, but all letters will be interpreted as uppercase. Punctuation
   will not be encrypted. The key will contain only letters (lower or upper
   case), but again will only be interpreted as uppercase.


   Examples:

   vigenere "baz" "foobar" => "GONCAQ"

   vigenere "Yadda" "Hello, world!" => "FEOOO, UOUOD!"
-}
vigenere :: String  -- input string (plain text)
         -> String  -- encryption key
         -> String  -- encrypted string
vigenere key input = convert ( sum2 (toOrd(toUpperLs input)) (toOrd(toUpperLs(layerKey input (key) (key)))) ) 

-- layerKey (length input) (key) (key)
sum2 [] [] = []
sum2 (x:xs) (y:ys) = (f x y):(sum2 xs ys)
   where f a b = if ((a >= 0) && (a <= 25)) then (a+b) `mod` 26
                  else a
                   
convert [] = []
convert (x:xs) = if (x <= 25) && (x >= 0) then (chr (x+65)):(convert xs)
                  else chr x:(convert xs)

toOrd [] = []
toOrd (x:ls) = (norm x):toOrd ls 
               where norm x = if ((a >= 65) && (a <= 90)) then (a - 65)
                              else a 
                              where a = ord x

toUpperLs [] = []
toUpperLs (x:xs) = toUpper x: toUpperLs xs

layerKey :: [Char] -> [Char] -> [Char] -> [Char]

layerKey (x:xs) (l:layer) original   | xs == [] = if isLetter x then [l] else [' ']
                                     | isLetter x && (layer /= []) = l:(layerKey xs layer original) 
                                     | not (isLetter x) && (layer /= []) = ' ':(layerKey xs (l:layer) original)
                                     | isLetter x && (layer == []) = l:(layerKey xs original original) 
                                     | not (isLetter x) && (layer == []) = ' ':(layerKey xs (l:layer) original)
                           


showOrd [] = []
showOrd (x:xs) = (ord x):(showOrd xs)