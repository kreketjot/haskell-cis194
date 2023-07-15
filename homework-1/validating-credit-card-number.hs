{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (div n 10) ++ [mod n 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = mod n 10 : toDigitsRev (div n 10)
  | otherwise = []

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x1 : x2 : xs) = x1 : (2 * x2) : doubleEverySecond xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEverySecond (toDigitsRev n)) `mod` 10 == 0
