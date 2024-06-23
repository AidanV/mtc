module Calc where

-- TODO: Figure out why Num does not work
calculate :: String -> Double -- (Num a, Read a, Show a) => String -> a
calculate =
  calcFromRPN . words

calcFromRPN :: [String] -> Double
calcFromRPN =
  head . foldl f []
  where
    f (x : y : ys) "*" = (x * y) : ys
    f (x : y : ys) "+" = (x + y) : ys
    f (x : y : ys) "-" = (y - x) : ys
    f (x : y : ys) "/" = (y / x) : ys
    f (x : y : ys) "^" = (y ** x) : ys
    f (x : xs) "ln" = log x : xs
    f (x : xs) "sqrt" = sqrt x : xs
    f xs numberString = read numberString : xs
