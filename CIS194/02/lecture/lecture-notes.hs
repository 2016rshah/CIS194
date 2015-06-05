-- From the CIS lecture 
--Algebraic Data Types
data Thing = Shoe
			| Ship
			| SealingWax
			| Cabbage
			| King
	deriving Show -- This lets GHCI be nice

shoe :: Thing
shoe = Shoe

listOThings :: [Thing]
listOThings = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe 		= True
isSmall SealingWax	= True
isSmall Ship		= False
isSmall	Cabbage		= True
isSmall King 		= False

isSmall2 :: Thing -> Bool
isSmall2 Ship		= False
isSmall2 King		= False
isSmall2 _			= True -- Pattern matching for anything that isn't Ship or King will return true

data FailableDouble = Failure
					| OK Double 
	deriving Show

--OK :: Double -> FailableDouble

ex01 = Failure
ex02 = OK 3.4


data Person = Person String Int Thing -- Person constructor takes a string name, an int age, and a favorite thing
	deriving Show


brent ::     Person
brent = Person "Brent" 31 SealingWax -- His favorite thing is SealingWax lol

getAge :: Person -> Int 
getAge (Person _ age _) = age

baz :: Person -> String
baz p@(Person n _ _) = "The name field of ("++show p++") is"++n
--the p@constructor gives you the value of everything in the constructor along with the entire object

--Case Expressions
{--
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
--}

ex03 = case "Hello" of
    [] -> 3
    ('H':s) -> length s --evaluates to this one
    _ -> 7 --Would evaluate to this, but it is already matched

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d















