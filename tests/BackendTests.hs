module Main where

import Calc
import qualified System.Exit as Exit
import Test.HUnit

testAddition :: Test
testAddition = TestCase (assertEqual "Calculating: 1 2 +" 3.0 $ calculate "1 2 +")

testSubtraction :: Test
testSubtraction = TestCase (assertEqual "Calculating: 3 2 -" 1.0 $ calculate "3 2 -")

testMultiplication :: Test
testMultiplication = TestCase (assertEqual "Calculating: 4 5 *" 20.0 $ calculate "4 5 *")

testDivision :: Test
testDivision = TestCase (assertEqual "Calculating: 6 3 /" 2.0 $ calculate "6 3 /")

testExponentiation :: Test
testExponentiation = TestCase (assertEqual "Calculating: 2 4 ^" 16.0 $ calculate "2 4 ^")

testNaturalLog :: Test
testNaturalLog = TestCase (assertEqual "Calculating: 2.718281828459045 ln" 1.0 $ calculate "2.718281828459045 ln")

testSqrt :: Test
testSqrt = TestCase (assertEqual "Calculating: 16 sqrt" 4.0 $ calculate "16 sqrt")

testAdditionWithVar :: Test
testAdditionWithVar = TestCase (assertEqual "Calculating: 1 a + where a = 10" 11.0 $ calculateWithVar ["10.0"] "1 a +")

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
      TestLabel "Test Addition w/ Var" testAdditionWithVar
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
