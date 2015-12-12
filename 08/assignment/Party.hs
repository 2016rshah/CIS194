module Party where

import Data.Monoid
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL gs f) = GL (emp:gs) (ef+f)

instance Monoid GuestList where
	mempty = GL [] 0 --Empty guest list
	mappend (GL gs1 f1) (GL gs2 f2) = GL (gs1 ++ gs2) (f1 + f2) 
	--Adding two guest lists together gives the addition of the lists and addition of fun


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2


