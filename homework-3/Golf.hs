{-# OPTIONS_GHC -Wall #-}

module Golf where

-- exercise 1

-- make square matrix
makeListOfLists :: [a] -> [[a]]
makeListOfLists xs = map (const xs) xs

-- map list to list of pairs: list item and corresponding serial number (1 for first list item, 2 for second list item, etc)
addSerialNumber :: [a] -> [(a, Int)]
addSerialNumber xs = zip xs [1 ..]

-- filter list of pairs by saving n-th elements (n is second parameter in pair)
filterBySerialNumber :: ([(a, Int)], Int) -> ([(a, Int)], Int)
filterBySerialNumber (lst, sn) = (filter (\(_, i) -> mod i sn == 0) lst, sn)

-- 1. make list of lists with the same length
-- 2. map every internal list to list of pairs
-- 3. map list to list of pairs
-- 4. filter every internal list: save only n-th elements
-- 5. remove serial numbers from list
-- 6. remove serial numbers from internal lists
skips :: [a] -> [[a]]
skips = map (map fst . fst . filterBySerialNumber) . addSerialNumber . map addSerialNumber . makeListOfLists

-- exercise 2

-- checks if second parameter is local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs)
  | b > a && b > c = b : localMaxima (b : c : xs)
  | otherwise = localMaxima (b : c : xs)
localMaxima _ = []

-- exercise 3

-- count occurrences of given number in list
countOccurrences :: Integer -> [Integer] -> Int
countOccurrences x = length . filter (== x)

-- count digit occurrences in given list
countDigits :: [Integer] -> [Int]
countDigits list = map (`countOccurrences` list) [0 .. 9]

-- get line depending on level and occurrences
histogramLine :: Int -> [Int] -> String
histogramLine lvl = map (\x -> if x >= lvl then '*' else ' ')

-- count levels and get line for each level
histogramLines :: [Int] -> [String]
histogramLines occs = map (`histogramLine` occs) (reverse [1 .. (maximum occs + 1)])

-- join lines with \n and add histogram footer
histogram :: [Integer] -> String
histogram digits = unlines (histogramLines $ countDigits digits) ++ "==========\n0123456789\n"
