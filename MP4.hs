module MP4 where

import Data.Char


data State s a = State { run :: s -> Maybe (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')


instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


instance Alternative (State s) where
  empty = State $ \_ -> Nothing
  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r


type Parser a = State String a


item :: Parser Char
item = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else State (\_ -> Nothing)


char :: Char -> Parser Char
char c = sat (==c)


string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return $ x:xs


space :: Parser ()
space = do many $ sat isSpace
           return ()


token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x


symbol :: String -> Parser String
symbol s = token (string s)

integer  :: Parser Int 
integer = token int 

digit :: Parser Char 
digit = sat isDigit 

digits :: Parser String
digits = some digit 

letter :: Parser Char 
letter = sat isLetter 

letters :: Parser String
letters = some letter

onePlus :: Parser a -> Parser [a]
onePlus p = some p

zeroPlus :: Parser a -> Parser [a]
zeroPlus p = onePlus p <|> return []

nat :: Parser Int 
nat = read <$> digits 

int :: Parser Int 
int = (do char '-'
          n <- nat
          return (-n))
      <|> nat

consumeTill :: Char -> Parser String 
consumeTill c =  do a <- sat (/= c) 
                    as <- consumeTill c <|> return []
                    return (a:as)

takeC :: Char -> Parser Char 
takeC c = do sat (==c)

check :: State s a  -> State s a
check p = State $ \s -> case run p s of
                          Nothing -> Nothing
                          r -> r
satC :: (Char -> Bool) -> Parser Char
satC p = do x <- itemC
            if p x then return x else State (\_ -> Nothing)

satF :: (Char -> Bool) -> Parser Char 
satF p = do x <- itemC
            if p x then return x else return '!' 

itemC :: Parser Char
itemC = State $ \input -> case input of 
                                "" -> Nothing
                                (x:xs) -> Just (x, x:xs)
failed :: State s a
failed = State $ \s -> Nothing 

typeName ::  Parser String
typeName = do space
              tp <- symbol "int" <|> symbol "char"
              return tp


varName :: Parser String
varName =  do space
              n <- letter <|> digit
              ns <- varName <|> return []
              return (n:ns)       



paramList :: Parser [String]
paramList = (do symbol "("
                typeName
                check (satC isLower)
                p <- varName  
                ps <- many (do symbol ","
                               typeName
                               check (satC isLower)
                               varName)
                symbol ")"
                return (p:ps)
              )<|> do symbol "("
                      symbol ")"
                      return [""]

intList :: Parser [Int]
intList = do symbol "["
             n <- integer   
             ns <- many (do symbol ","
                            integer)
             symbol "]"
             return (n:ns)                                 



assignment :: [String] -> [String] ->  Parser [()]
assignment ps vs = many ((do a <- varName
                             if a `elem` ps || a `elem` vs then 
                              do symbol "="
                                 b <- varName
                                 if b `elem` ps || b `elem` vs then
                                   do symbol ";"
                                      return ()
                                 else failed 
                             else failed)
                          <|> (do x <- varName
                                  if x `elem` ps || x `elem` vs then 
                                     do symbol "="
                                        y <- integer
                                        symbol ";"
                                        return ()
                                  else failed)) 

varDecls :: Parser [[String]]
varDecls = many (do typeName
                    check (satC isLower)
                    p <- varName  
                    ps <- many (do symbol ","
                                   satC isLower
                                   varName)
                    symbol ";"
                    return (p:ps)) <|> do return [["!"]]

returnQ :: [String] -> [String] -> Parser String
returnQ paraL varsL = (do symbol "return"
                          a <- varName
                          if a `elem` paraL || a `elem` varsL then
                            do symbol ";"
                               symbol "}"
                               return a
                          else   failed)
                 <|>(do symbol "return"
                        a <- integer
                        symbol ";"
                        symbol "}"
                        return (show a))
                 <|>(do symbol "}"
                        return "")

funcDef :: Parser (String,[String],[String],String)
funcDef = do typeName
             check (satC isLower)
             n <- varName
             paramL <- paramList  
             let paraL = filter (/="") paramL
             symbol "{"
             vars <- varDecls
             let varsL = concat vars 
             assignment paraL varsL
             rq <- returnQ paraL varsL 
             return (n, paraL, varsL, rq)


{-
  Parses a limited C function in order to obtain:

  1. The name of the function
  2. A list of the names of the parameters of the function
  3. A list of the names of the local variables declared within the function
  4. The variable name or integer value returned by the function (as a string),
     or the empty string if there is no return statement.

  See the writeup for examples.
-}





