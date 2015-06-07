import Data.List
import Test.HUnit
import Golf
{--
Tests
--}
ex02Tests = TestList [
    [9,6] ~=? localMaxima [2,9,5,6,1],
    [4] ~=? localMaxima [2,3,4,1,5],
    [] ~=? localMaxima [1,2,3,4,5]
    ]
main = do
	--runTestTT ex01Tests
    runTestTT ex02Tests
    --runTestTT ex03Tests
    return ()