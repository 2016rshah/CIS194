--http://www.cs.toronto.edu/~liudavid/csc324/extra/HUnit.html

import Test.HUnit
import Part1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate)

ex1Tests = TestList [
    [1, 2, 3, 4] ~=? toDigits 1234,
    [4,3,2,1] ~=? toDigitsRev 1234,
    [] ~=? toDigits 0,
    [] ~=? toDigits (-17)
    ]

ex2Tests = TestList [
	[16,7,12,5] ~=? doubleEveryOther [8,7,6,5],
	[1,4,3] ~=? doubleEveryOther [1,2,3]
	]

ex3Tests = TestList [
	22 ~=? sumDigits [16,7,12,5]
	]

ex4Tests = TestList [
	True ~=? validate 4012888888881881,
	False~=? validate 4012888888881882
	]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT ex1Tests
    runTestTT ex2Tests
    runTestTT ex3Tests
    runTestTT ex4Tests
    return ()