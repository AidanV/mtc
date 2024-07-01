module Calc where

import Text.Read

calculateWithVar :: [String] -> String -> Maybe Double
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
calculate :: String -> Maybe Double -- (Num a, Read a, Show a) => String -> a
calculate s =
  calcFromRPN (words s)

calcFromRPN :: [String] -> Maybe Double
calcFromRPN =
  head . foldl f []
  where
    f (Just x : Just y : ys) "*" = Just (x * y) : ys
    f (Just x : Just y : ys) "+" = Just (x + y) : ys
    f (Just x : Just y : ys) "-" = Just (y - x) : ys
    f (Just x : Just y : ys) "/" = Just (y / x) : ys
    f (Just x : Just y : ys) "^" = Just (y ** x) : ys
    f (Just x : xs) "ln" = Just (log x) : xs
    f (Just x : xs) "sqrt" = Just (sqrt x) : xs
    f xs numberString = (readMaybe ns :: Maybe Double) : xs
      where
        ns =
          if head numberString == '.'
            then '0' : numberString
            else numberString
