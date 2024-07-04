module Calc where

import Text.Read

calculateWithVar :: [String] -> String -> Maybe Double
calculateWithVar prevAns eq =
  let variables = zip (map (\x -> '#' :[x]) ['a' ..]) prevAns
      replacedString = foldl (\acc (i, prevAns) -> replace i prevAns acc) eq variables
  in calculate replacedString
  where 
    replace from to [] = []
    replace from to s@(x:xs) = 
          if take (length from) s == from 
            then to ++ replace from to (drop (length from) s)
            else x : replace from to xs 



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
          (fixStart . fixEnd) numberString
        fixStart n = 
          if head n == '.'
            then '0' : n
            else n
        fixEnd n =
          if last n == '.'
            then n ++ "0"
            else n
