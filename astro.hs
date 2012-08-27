module Main where

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)
  
fac' :: Integer -> Integer
fac' n =
  case n of
    1 -> 1
    _ -> n * fac (n - 1)
  
fac'' :: Integer -> Integer
fac'' n
    | n <= 1    = 1
    | otherwise = n * fac (n - 1)
  
fac''' :: Integer -> Integer
fac''' n =
  if n <= 1
  then 1
  else n * fac (n - 1)

facs = 1 : go 2
  where go :: Integer -> [Integer]
        go n = n : map (* n) (go (n + 1))
