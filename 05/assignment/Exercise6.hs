{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import Expr

--Exercise 6

class HasVars a where
  var :: String -> a

--Enumerates all the different constructors for a VarExprT
data VarExprT = Lit Integer
              | Mul VarExprT VarExprT
              | Add VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

--These two instances just pass arguments to the constructors
instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

--Give a type synonym to the given type from the assignment
type MapExpr = (M.Map String Integer -> Maybe Integer) -- This means that if you return MapExpr you need to return a function!

--Not entirely clear on this one (?)
instance HasVars MapExpr where
  var i m  = M.lookup i m  -- Could curry but this is more useful

--Arg 1 -> function like + or *
--Arg 2 -> first maybe value
--Arg 3 -> second maybe value
--Return-> function on two maybes if neither are nothing
fixMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
fixMaybes f Nothing _ = Nothing
fixMaybes f _ Nothing = Nothing
fixMaybes f (Just x) (Just y) = Just (f x y)

instance Expr MapExpr where
  lit z = (\_ -> Just  z)
  add f g = (\o -> fixMaybes (+) (f o) (g o)) 
  mul f g = (\o -> fixMaybes (*) (f o) (g o))

--Given
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
