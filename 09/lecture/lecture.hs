--Kinds describe types
--The first level is things themselves. They are their type
--For example
> :k Int
Int :: *

> :k Bool
Bool :: *

> :k Maybe Int
Maybe Int :: *

--Based on this, what is the kind of Maybe? It takes a * and creates a * so it must be
> :k Maybe
Maybe :: * -> *

--In other words, it is a function on a kind

--A functor is basically like a container
--The functor typeclass defines `fmap`
class Functor f where
    fmap :: (a -> b) -> f a -> f b

--Thus to implement the typeclass, you must be able to apply a function to ever element that is being stored in the container

--However, there are other soft requirements for functors called the functor laws (which are laws for the mathematical functor that Haskell's functor was based on

--The two laws are
fmap id = id -- Basically calling the id function on every element should return an isomorphism of the original thing
fmap (g . h) = (fmap g) . (fmap h) -- Calling the composition of two functions on a thing is the same as calling the first function, then the second one

--In general, if you satisfy the first law, you'll typically also automatically satisfy the second one.

--An example of a "valid" functor that doesn't satisfy the functor laws:
-- Evil Functor instance
instance Functor [] where
    fmap _ [] = []
    fmap g (x:xs) = g x : g x : fmap g xs --bad, because it doubles g x which means that id x : id x is not the same as id x

--There is only one valid instance of functor! Like its been legit proven that there is only one valid functor instance. How cool is that??



                                                                                


                                                                                  
