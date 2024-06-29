module Calc where

calculateWithVar :: [String] -> String -> Double
calculateWithVar prevAns eq =
  let variables = zip ['a' ..] prevAns
      replacedString = foldl (\acc (c, prevAns) -> replaceCharWithString c prevAns acc) eq variables
   in calculate replacedString

replaceCharWithString :: Char -> String -> String -> String
replaceCharWithString c replacement [] =
  []
replaceCharWithString c replacement (firstChar : rest) =
  if firstChar == c
    then replacement ++ replaceCharWithString c replacement rest
    else firstChar : replaceCharWithString c replacement rest

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
