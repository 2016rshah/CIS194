{-# LANGUAGE FlexibleInstances #-}

-- Parametric polymorphism 
-- Works uniformly for any type chosen by the caller
-- For example
f :: a -> a -> a -- Can take two variables of the same type and return a value of same type regardless of what that type is
f x y = x

f' :: a -> a -> a
f' x y = y 

-- f and f' are the only two functions that have that type signature

{--
Let’s play the parametricity game! Consider each of the following polymorphic types. For each type, determine what behavior(s) a function of that type could possibly have.

a -> a
-- id function that returns the parameter it was passed

a -> b
-- function that changes the type of the variable it was passed

a -> b -> a
-- function that returns the first parameter

[a] -> [a]
-- basically any function on a list like reverse

(b -> c) -> (a -> b) -> (a -> c)
-- composition of functions

(a -> a) -> a -> a
-- function that applies function in first argument to second argument
--}

-- with an ambigious type declaration like 
-- a -> a
-- Any type needs to work. 

-- But in order to restrict it to working with any type that has certain properties you use the => like this
-- Num => a -> a -> a
-- This example would be used for something like adding

-- This thick arrow thing (=>) is a way to restrict a type signature to a "type class"
-- Using the arrow thing makes a function "type class polymorphic"
-- Type classes refer to a set of types that have certain functions defined for them 
-- For example the Eq class would need equal and not equal to be defined
--class Eq a where
--	(==) :: a -> a -> Bool
--	(/=) :: a -> a -> Bool
-- This means that the type signature for (==) would be 
-- (==) :: Eq a => a -> a -> Bool

-- A better implementation of Eq is like this
--class Eq a where
--  (==), (/=) :: a -> a -> Bool
--  x == y = not (x /= y)
--  x /= y = not (x == y)
-- This defines == and /= in terms of each other so you only need to define one. 

-- In order to use a type class you need to define an instance of that type class
-- For example, Int is an instance of Num
-- Another example is layed out below
data Foo = F Int | G Char

instance Eq Foo where
	(==) (F x) (F y) = x == y
	(==) (G x) (G y) = x == y 
	(==) _ _ = False -- F can not equal G

-- Certain type classes are special and GHC can automatically derive their instances for you
-- For example
data FooExample = FExample Int | GExample Char
	deriving (Eq, Ord, Show)


-- Important syntax examples:

--toList :: Listable a => a -> [Int]
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id


foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y