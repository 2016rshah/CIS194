{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser

--Exercise 1 
eval :: ExprT -> Integer
eval (Lit z) = z
eval (Mul x y) = eval x * eval y 
eval (Add x y) = eval x + eval y 

--Exercise 2
evalStr :: String -> Maybe Integer
evalStr = maybe Nothing (Just . eval) . parseExp Lit Add Mul

--Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer 	-- Just (-7)
instance (Expr Integer) where
	add x y = x + y 
	mul x y = x * y 
	lit = id

testBool = testExp :: Maybe Bool 		-- Just True
instance (Expr Bool) where
	add x y = x || y
	mul x y = x && y 
	lit z
		| z > 0 = True
		| otherwise = False

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord) -- Added Ord to typeclasses
testMM = testExp :: Maybe MinMax		-- Just (MinMax 5)
instance (Expr MinMax) where
	add = max
	mul = min
	lit = MinMax

--I'm not sure I entirely understood this task. 
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
testSat = testExp :: Maybe Mod7 			-- Just (Mod7 0)
instance (Expr Mod7) where
	lit z 
		| z >= 0 && z < 7 = Mod7 z
		| otherwise = lit (z `mod` 7)
	add (Mod7 x) (Mod7 y) = lit (x + y)
	mul (Mod7 x) (Mod7 y) = lit (x * y)
