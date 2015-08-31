{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc6 where

import Calc
import qualified Data.Map as M


--Type class
class HasVars a where
	var :: String -> a

--New Type
data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String 
  deriving (Show, Eq)

--To define a type that is of multiple type classes just make it an instance of each type class separately:

--Type is an instance of typeclass. 
instance HasVars VarExprT where
	var s = Calc6.Var s

instance Expr VarExprT where
	lit = Calc6.Lit
	add = Calc6.Add
	mul = Calc6.Mul

type MapExprT = M.Map String Integer -> Maybe Integer

-- var :: String -> a
-- var :: String -> MapExprT
-- var :: String -> M.Map String Integer -> Maybe Integer
-- var s m = --lookup string (s) in map (m)
-- var s m = M.lookup s m 
instance HasVars MapExprT where
	var s m = M.lookup s m 

-- If anything is Nothing, then it is Nothing. Otherwise wrap it in a Just
fixMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
fixMaybes _ Nothing _ = Nothing
fixMaybes _ _ Nothing = Nothing
fixMaybes o (Just a) (Just b) = Just (o a b)

-- lit :: Integer -> M.Map String Integer -> Maybe Integer
instance Expr MapExprT where
	lit i = (\_ -> Just i)  
	add a b = (\m -> fixMaybes (+) (a m) (b m)) 
	mul a b = (\m -> fixMaybes (*) (a m) (b m))

--a and b are constructors :: M.Map String Integer -> Maybe Integer 
--give them the map and they might give you an integer

--Given:
withVars :: [(String, Integer)]
	-> (M.Map String Integer -> Maybe Integer)
	-> Maybe Integer
withVars vs ex = ex $ M.fromList vs