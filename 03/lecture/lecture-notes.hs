--Haskell lets you abstract away common recursion patterns like 
--map - apply a function to each element 
--filter - get only elements that match a case 
--folds - summarizes a list into one value like sum product etc

--Polymorphism
data IntList = Empty | Cons Int IntList deriving Show --not polymorphic -> bad
data List t = E | C t (List t) --polymorphic which is better
--t is a type variable (always start with lowercase letter while types start uppercase)
--List type is a paramaterized type by t 

filterList :: (t -> Bool) -> List t -> List t
--Takes a function that takes a value of type t and returns a boolean, along with a list of values of type t, and returns a list of type t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

--The type declaration for mapList:
--Initially you might think this is good: mapList :: (t -> t) -> List t -> List t
--But that's bad because that means you have to return a list of the type you gave the function
--Instead it is better to do this:  mapList :: (a -> b) -> List a -> List b
mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

data Maybe a = Nothing | Just a
--Graceful way to handle errors

--partial functions do not work for all inputs unlike total functions which do. 
--One should replace common partial functions with pattern matching:
--You should almost never use head, tail, init, last, or (!!)

--If you find yourself having to write partial functions you should make it clear with the type system using Maybe
--rewriting the partial function head safely would look like this:
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

--Even if you think that you will only call a function with certain conditions the type system should reflect that partiality