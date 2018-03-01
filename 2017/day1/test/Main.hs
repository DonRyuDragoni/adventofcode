import Test.HUnit

import Lib

-- First half of the problem

example1 = TestCase $ assertEqual "1122 => 3"
  3
  (solveCaptcha "1122")

example2 = TestCase $ assertEqual "1111 => 4"
  4
  (solveCaptcha "1111")

example3 = TestCase $ assertEqual "1234 => 0"
  0
  (solveCaptcha "1234")

example4 = TestCase $ assertEqual "91212129 => 9"
  9
  (solveCaptcha "91212129")

-- Part two

example5 = TestCase $ assertEqual "1212 => 6"
  6
  (solveCaptcha "1212")

example6 = TestCase $ assertEqual "1221 => 0"
  0
  (solveCaptcha "1221")

example7 = TestCase $ assertEqual "123425 => 4"
  4
  (solveCaptcha "123425")

example8 = TestCase $ assertEqual "123123 => 12"
  12
  (solveCaptcha "123123")

example9 = TestCase $ assertEqual "12131415 => 4"
  4
  (solveCaptcha "12131415")

tests = TestList
  [ TestLabel "First example"   example1
  , TestLabel "Second example"  example2
  , TestLabel "Third example"   example3
  , TestLabel "Fourth example"  example4

  -- Part two
  , TestLabel "Fifth example"   example5
  , TestLabel "Sixth example"   example6
  , TestLabel "Seventh example" example7
  , TestLabel "Eighth example"  example8
  , TestLabel "Ninth example"   example9
  ]

main :: IO ()
main = do
  runTestTT tests
  putStr ""
