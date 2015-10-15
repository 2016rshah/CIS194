module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (m1 `mappend` m2) x y 
	where 
		m1 = tag x
		m2 = tag y


tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]     	!!? _         	= Nothing
_		!!? i | i < 0 	= Nothing
(x:xs) 	!!? 0 			= Just x 
(x:xs) 	!!? i 			= xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

--size is the sum of all the size annotations of the subtrees

sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                 	= Nothing
indexJ i _     | i < 0         	= Nothing
indexJ i jl    | i > sz jl     	= Nothing
indexJ 0 (Single _ a)          	= Just a 
indexJ _ (Single _ _)			= Nothing
indexJ i (Append m left right)
  | i < sz left = indexJ i left
  | otherwise   = indexJ (i - sz left) right

testIndex :: (Sized b, Monoid b, Eq a) => Int -> JoinList b a -> Bool
testIndex i jl = (indexJ i jl) == (jlToList jl !!? i)

--What confuses me the most is what in the world does this tree look like?
--For now I'm going with this: `let jl = (Append (Size 3) (Single (Size 1) Empty) (Single (Size 1) Empty))`

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ x jl | x <= 0 = jl 
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append m left right)
	| i < sz left = dropJ i left
	| otherwise = dropJ (i - sz left) right

testDrop :: (Sized b, Monoid b, Eq a) => Int -> JoinList b a -> Bool
testDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)


--takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

--testTake :: (Sized b, Monoid b, Eq a) => Int -> JoinList b a -> Bool
--testTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)