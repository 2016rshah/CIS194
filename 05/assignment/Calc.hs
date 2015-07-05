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