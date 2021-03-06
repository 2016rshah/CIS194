{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT as T
import Parser
import StackVM as VM

--Exercise 1 
eval :: ExprT -> Integer
eval (T.Lit z) = z
eval (T.Mul x y) = eval x * eval y 
eval (T.Add x y) = eval x + eval y 

--Exercise 2
evalStr :: String -> Maybe Integer
evalStr = maybe Nothing (Just . eval) . parseExp T.Lit T.Add T.Mul

--Exercise 3
class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a

instance Expr ExprT where
	lit = T.Lit
	add = T.Add
	mul = T.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp 	-- Just (-7)
instance (Expr Integer) where
	add x y = x + y 
	mul x y = x * y 
	lit = id

testBool :: Maybe Bool
testBool = testExp 		-- Just True
instance (Expr Bool) where
	add x y = x || y
	mul x y = x && y 
	lit z
		| z > 0 = True
		| otherwise = False

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord) -- Added Ord to typeclasses
testMM :: Maybe MinMax
testMM = testExp		-- Just (MinMax 5)
instance (Expr MinMax) where
	add = max
	mul = min
	lit = MinMax

--I'm not sure I entirely understood this task. 
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
testSat :: Maybe Mod7
testSat = testExp 			-- Just (Mod7 0)
instance (Expr Mod7) where
	lit z 
		| z >= 0 && z < 7 = Mod7 z
		| otherwise = lit (z `mod` 7)
	add (Mod7 x) (Mod7 y) = lit (x + y)
	mul (Mod7 x) (Mod7 y) = lit (x * y)

--Exercise 5

instance Expr VM.Program where
	lit z = [VM.PushI z] 
	add x y = x ++ y ++ [VM.Add] 
	mul x y = x ++ y ++ [VM.Mul]

testProg :: Maybe VM.Program
testProg = testExp

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
