module Party where

import Data.Monoid
import Data.Tree
import Data.List

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL gs f) = GL (emp:gs) (ef+f)

instance Monoid GuestList where
	mempty = GL [] 0 --Empty guest list
	mappend (GL gs1 f1) (GL gs2 f2) = GL (gs1 ++ gs2) (f1 + f2) 
	--Adding two guest lists together gives the addition of the lists and addition of fun


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2

moreFun' :: (GuestList, GuestList) -> GuestList
moreFun' (g1@(GL _ f1), g2@(GL _ f2)) = if f1 > f2 then g1 else g2


--Don't entirely understand this
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root ts) = (f root) (map (treeFold f) ts)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
--Employee is the boss
--List of possiblities. For each possibility, the first Guest list includes the boss, the second does not
--Return the best guest list that includes the boss, and the best guest list that doesn't include the boss
--Add employee to each of the second ones, that is your second result
--Find the max of the first one, that is your first result
nextLevel boss@(Emp {empFun = ef}) xs@(x:s) = (withEmp, withoutEmp)
	where 
		withoutEmp = mconcat (map moreFun' xs)
		--withoutEmp basically adds up (mconcat) all the subtrees because they're all invited. 
		--The resason you use moreFun' rather than just fst is because if you have more fun by not inviting the sub-boss, then you should do it
		withEmp = glCons boss (mconcat (map snd xs))
		--withEmp basically adds up (mconcat) all the subtrees without the sub-boss because they're all invited, and then adds the boss once. 
nextLevel boss _ = ((glCons boss mempty), mempty)
--If there's an empty guest list right now, you need to return a guestlist with the boss or an empty guest list. 

--Correct answer is:
--GL [Emp {empName = "John", empFun = 1},Emp {empName = "Sue", empFun = 5},Emp {empName = "Fred", empFun = 3},Emp {empName = "Sarah", empFun = 17}] 26

maxTuple :: Ord a => (a, a) -> a
maxTuple (x, y) = if x > y then x else y

maxFun :: Tree Employee -> GuestList
maxFun tree = maxTuple (treeFold nextLevel tree)

showGuestList :: [Employee] -> String
showGuestList = intercalate "\n" . sort . map empName 

main = do
	inp <- readFile "company.txt"
	let tree = read inp :: Tree Employee
	let (GL guests totalFun) = maxFun tree --This is pretty fancy. You can save the pattern matching when you assign to a variable
	putStrLn ("Total fun: " ++ (show totalFun))
	putStrLn (showGuestList guests)