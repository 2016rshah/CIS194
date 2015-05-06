import Test.QuickCheck
import Data.List
import Test.HUnit

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs


prop_idempotent :: [Integer] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

isPrime :: Int -> Bool
isPrime a = [] == [x | x <- [2..a-1], a `mod` x == 0]

primes :: [Int]
primes = [x | x<-[2..], isPrime x]

isPrimeTests = TestList [
    True ~=? isPrime 3,
    False ~=? isPrime 20
    ]

primesTests = TestList [
    2 ~=? head primes
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT isPrimeTests
    runTestTT primesTests
    return ()