import Data.List
import Test.HUnit
import Golf
{--
Tests
--}

ex01Tests = TestList [
    ["ABCD", "BD", "C", "D"] ~=? skips "ABCD",
    ["hello!", "el!", "l!", "l", "o", "!"] ~=? skips "hello!",
    [[1]] ~=? skips [1],
    [[True,False], [False]] ~=? skips [True,False]
    ]

ex02Tests = TestList [
    [9,6] ~=? localMaxima [2,9,5,6,1],
    [4] ~=? localMaxima [2,3,4,1,5],
    [] ~=? localMaxima [1,2,3,4,5]
    ]

main = do
    runTestTT ex01Tests
    runTestTT ex02Tests
    --no tests for third exercise because textual representation is more effort than its worth
    return ()