module Expr where

class Expr a where
	lit :: Integer -> a
	add :: (Expr a) => a -> a -> a
	mul :: (Expr a) => a -> a -> a