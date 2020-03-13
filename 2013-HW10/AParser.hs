{-# OPTIONS_GHC -Wall #-}
{-# Language InstanceSigs #-}
import           Control.Applicative

import           Data.Char
-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
 where
  f [] = Nothing    -- fail on the empty input
  f (x : xs) |          -- check if x satisfies the predicate
                      -- if so, return x along with the remainder
                      -- of the input (that is, xs)
               p x       = Just (x, xs)
             | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
 where
  f xs | null ns   = Nothing
       | otherwise = Just (read ns, rest)
    where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- Exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap a2b (Parser fa) = Parser (fmap (first a2b) . fa)

-- Exercise 2
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\xs -> Just (a, xs))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser
    (\str -> case runParser p1 str of
      Nothing            -> Nothing
      Just (a2b, remain) -> runParser (fmap a2b p2) remain
    )

type Name = String
data Employee = Emp { name :: Name, phone :: String }
    deriving (Show)

parseName :: Parser Name
parseName = Parser f
 where
  f xs = case words xs of
    a : b -> Just (a, unwords b)
    _     -> Nothing

parsePhone :: Parser String
parsePhone = Parser f
 where
  f xs = case runParser posInt xs of
    Just (num, rest) -> Just (show num, rest)
    _                -> Nothing

parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone

exercise2 :: IO ()
exercise2 = print $ runParser parseEmployee "Jhon 01000001111rest"

-- Exercise3
abParser :: Parser (Char, Char)
abParser = (\c1 c2 -> (c1, c2)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\i1 _ i2 -> [i1, i2]) <$> posInt <*> char ' ' <*> posInt

exercise3 :: IO ()
exercise3 = do
  print $ runParser abParser "abcdef"
  print $ runParser abParser "aebcdef"
  print $ runParser abParser_ "abcdef"
  print $ runParser abParser_ "aebcdf"
  print $ runParser intPair "12 34"

-- Exercise4
instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\str -> runParser p1 str <|> runParser p2 str)

-- Exercise5
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)

exercise4 :: IO ()
exercise4 = do
  print $ runParser intOrUppercase "342abcd"
  print $ runParser intOrUppercase "XYZ"
  print $ runParser intOrUppercase "foo"


-- Main
main :: IO ()
main = do
  exercise2
  exercise3
  exercise4
