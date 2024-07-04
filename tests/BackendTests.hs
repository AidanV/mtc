module Main where

import Calc
import qualified System.Exit as Exit
import Test.HUnit

testAddition :: Test
testAddition = TestCase (assertEqual "Calculating: 1 2 +" (Just 3.0) $ calculate "1 2 +")

testSubtraction :: Test
testSubtraction = TestCase (assertEqual "Calculating: 3 2 -" (Just 1.0) $ calculate "3 2 -")

testMultiplication :: Test
testMultiplication = TestCase (assertEqual "Calculating: 4 5 *" (Just 20.0) $ calculate "4 5 *")

testDivision :: Test
testDivision = TestCase (assertEqual "Calculating: 6 3 /" (Just 2.0) $ calculate "6 3 /")

testExponentiation :: Test
testExponentiation = TestCase (assertEqual "Calculating: 2 4 ^" (Just 16.0) $ calculate "2 4 ^")

testNaturalLog :: Test
testNaturalLog = TestCase (assertEqual "Calculating: 2.718281828459045 ln" (Just 1.0) $ calculate "2.718281828459045 ln")

testSqrt :: Test
testSqrt = TestCase (assertEqual "Calculating: 16 sqrt" (Just 4.0) $ calculate "16 sqrt")

testAdditionWithVar :: Test
testAdditionWithVar = TestCase (assertEqual "Calculating: 1 #a + where #a = 10" (Just 11.0) $ calculateWithVar ["10.0"] "1 #a +")

testPointOne :: Test
testPointOne = TestCase (assertEqual "Calculate: .1" (Just 0.1) $ calculate ".1")

testOnePoint :: Test
testOnePoint = TestCase (assertEqual "Calculate: 1." (Just 1.0) $ calculate "1.")

tests :: Test
tests =
  TestList
    [ TestLabel "Test Addition" testAddition,
      TestLabel "Test Subtraction" testSubtraction,
      TestLabel "Test Multiplication" testMultiplication,
      TestLabel "Test Division" testDivision,
      TestLabel "Test Exponentiation" testExponentiation,
      TestLabel "Test Natual Log" testNaturalLog,
      TestLabel "Test Sqrt" testSqrt,
      TestLabel "Test Addition w/ Var" testAdditionWithVar,
      TestLabel "Test .1" testPointOne,
      TestLabel "Test 1." testOnePoint
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
