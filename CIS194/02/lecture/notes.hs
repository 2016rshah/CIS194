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

ex01 = Failure
ex02 = OK 3.4
