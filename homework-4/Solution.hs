{-# OPTIONS_GHC -Wall #-}

-- exercise 1

-- take even numbers, subtract 2 from them, product them
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> [Integer]
fun2' = takeWhile even $ iterate ((+1) . (*3))