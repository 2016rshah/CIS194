import Data.Monoid

--You can fold most data structures
--For example, fold trees!

data Tree a = Empty 
			| Node (Tree a) a (Tree a) 
	deriving (Show, Eq) 

leaf :: a -> Tree a 
leaf x = Node Empty x Empty 

treeSize :: Tree a -> Integer
treeSize Empty = 0 
treeSize (Node l _ r) = 1 + (treeSize l) + (treeSize r)

--And so on with
--treeSum :: Tree Integer -> Integer

treeDepth :: Tree a -> Integer
treeDepth Empty = 0 
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

--flatten :: Tree a -> [a]

--But if you define a fold like this:

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b -- The first arguments of the type signature all represent the constructors
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r) -- why do l and r represent themselves with the function applied?

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r) 

--Doesn't just have to be Trees, it can also be custom data types like ExprT
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


--The fold for T will take one (higher-order) argument for each of T’s constructors
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f g h (Lit i) = f i --f stands for the lit constructor
exprTFold f g h (Add a b) = g (exprTFold f g h a) (exprTFold f g h b) -- g stands for the Add constructor
exprTFold f g h (Mul a b) = h (exprTFold f g h a) (exprTFold f g h b) -- h stands for the Mul constructor

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

--Monoids
--A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function. 
--When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value. 
--1 is the identity with respect to * and [] is the identity with respect to ++.

--In other words
--If a function on a type is associative, and has a base case, it can be a monoid. 
-- + is a monoid because it is associative and it's base case is 0

--By associative, I am referring to the associative property that middle school made such a big deal out of
--Multiplication, addition, and even concatenation of lists are associative
--`mappend` is the function that needs to be associative

--By base case I mean that if you apply the monoid to the base case a variable x, it should return just x
--Like x + 0 returns 0 and y + 0 returns 0. Anything + 0 returns 0
--`mempty` is the base case

--`mconcat` is given for free when you define a monoid, because it is just a fold on `mempty` and `mappend`

--There was something in CIS194 about `<>` being a synonym for `mappend` but I can't find any more info on that

--Practice, make a few instances of Monoid for Bools. Logic gates

newtype And = And { a :: Bool }
  deriving (Eq, Show)

instance Monoid And where
	mempty = And True --default Bool is True
	mappend (And x) (And y) = And (x && y)

newtype Or = Or { o :: Bool}
	deriving (Eq, Show)

instance Monoid Or where
	mempty = Or True
	mappend (Or x) (Or y) = Or (x || y)