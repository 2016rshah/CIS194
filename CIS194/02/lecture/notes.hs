-- From the CIS lecture 
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


data Person = Person String Int Thing -- Person constructor takes a string name, an int age, and a favorite thing
	deriving Show


brent :: Person
brent = Person "Brent" 31 SealingWax -- His favorite thing is SealingWax lol

getAge :: Person -> Int 
getAge (Person _ age _) = age


--From RealWorldHaskell suggested reading
data DataTypeName = Constructor1Name attribute1 -- The DataTypeName and ConstructorName are usually the same if only one constructor.
	| Constructor2Name attribute1 attribute2 
	deriving Show

type CustomerID = Int --Type synonyms are purely to make code more readable

--This is one way to do boilerplate to get attributes but the way below is better.
--data Customer = Customer Int String [String]
--                deriving (Show)

--customerID :: Customer -> Int
--customerID (Customer id _ _) = id

--customerName :: Customer -> String
--customerName (Customer _ name _) = name

--customerAddress :: Customer -> [String]
--customerAddress (Customer _ _ address) = address

--This is called record syntax which generates the boilerplate functions above automagically
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

--Instantiate a customer
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

--If we use record-syntax we can specify attributes out of order and make code look nicer
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }

--Left off at `Parameterised types` in real world haskell














