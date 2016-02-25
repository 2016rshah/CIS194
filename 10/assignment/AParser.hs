{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

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
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
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
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first g (x, y) = (g x, y)

second :: (b -> c) -> (a,b) -> (a,c)
second g (x, y) = (x, g y)

instance Functor Parser where
  fmap g (Parser parserFunction) = Parser (fmap (first g) . parserFunction)
  --basically, you need to first run the function in Parser and then apply the function to the result
  --Therefore the instance of functor returns a partially applied function

instance Applicative Parser where
  pure a = Parser (\str -> Just (a, str))
  p1 <*> p2 = Parser f
              where
                f str = case runParser p1 str of --using case structure lets you pattern match result of running parser 1
                  Nothing -> Nothing
                  (Just (g, rem)) -> fmap (first g) (runParser p2 rem)
                  --fmap for the Maybe in the result of running parser2
                  --first for applying the function (g) to the parsed part of parser2
--What you need to realize is that p1 and p2 do NOT have the same type signature
--When you run the parser in p1, its parsed portion will be a function
--When you run the parser in p2, its parsed portion will be a value

abParser :: Parser (Char, Char)
abParser = (\a b -> (a, b)) <$> (char 'a') <*> (char 'b')
{-
runParser abParser "abcdef"
Just ((’a’,’b’),"cdef")
-}
--The returned parser needs to have its parsed value as a tuple of two characters

abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\a b c -> [a, c]) <$> (posInt) <*> (char ' ') <*> posInt 

instance Alternative Parser where
  empty = Parser (\str -> Nothing)
  p1 <|> p2 = Parser (\str -> f str)
              where f s = case runParser p1 s of
                      Nothing -> runParser p2 s
                      x -> x --if its not nothing, return it

intOrUppercase :: Parser ()
intOrUppercase = (\_ -> ()) <$> (e <$> posInt) <|> (e <$> (satisfy isUpper))
  where e _ = ()
