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
