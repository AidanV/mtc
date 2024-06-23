module Backend where

-- TODO: Figure out why Num does not work
calculate :: String -> Double --(Num a, Read a, Show a) => String -> a
calculate = 
    calcFromRPN . words
    
calcFromRPN :: [String] -> Double
calcFromRPN =
    head . foldl foldingFunction []
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs
    