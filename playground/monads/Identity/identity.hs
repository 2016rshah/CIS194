--http://blog.sigfpe.com/2007/04/trivial-monad.html
import Control.Applicative
data W x = W x deriving Show
                        
instance Functor W where
  fmap f (W x) = W (f x)

instance Applicative W where -- my own exercise
  pure x = W x
  (W g) <*> (W x) = W (g x) 
  
instance Monad W where
  return x = W x
  W x >>= f = f x

f :: Int -> W Int
f x = W (x + 1)

g :: Int -> W Int -> W Int
--g x (W y) = W (x+y)
g x y = y >>= (return . ((+) x))


h :: W Int -> W Int -> W Int
--h (W x) (W y) = W (x+y)
h x y = x >>= (\xx -> g xx y)

j :: W (W a) -> W a --join
j x = x >>= id 
