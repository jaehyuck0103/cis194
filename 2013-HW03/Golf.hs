{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
skipN :: Int -> [a] -> [a]
skipN n xs = case drop n xs of
  []       -> []
  (y : ys) -> y : skipN n ys

skips :: [a] -> [[a]]
skips xs = map (\x -> skipN x xs) [0 .. (length xs - 1)]

exercise1 :: IO ()
exercise1 = do
  print $ skips "ABCD" == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips [1 :: Integer] == [[1]]
  print $ skips [True, False] == [[True, False], [False]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : x2 : x3 : xs)
  | x2 > x1 && x2 > x3 = x2 : localMaxima (x2 : x3 : xs)
  | otherwise          = localMaxima (x2 : x3 : xs)
localMaxima _ = []

exercise2 :: IO ()
exercise2 = do
  print $ localMaxima [2, 9, 5, 6, 1] == [9, 6]
  print $ localMaxima [2, 3, 4, 1, 5] == [4]
  print $ localMaxima [1, 2, 3, 4, 5] == []

-- Exercise 3 -- 어렵다.. 그냥 github 뺏김..
-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0 .. 9]

-- return one * line
line :: [Int] -> Int -> String
line xs n = [ if i >= n then '*' else ' ' | i <- xs ]

histogram :: [Integer] -> String
histogram xs =
  unlines (map (line c) [m + 1, m .. 1]) ++ "==========\n0123456789\n"
 where
  c = count xs
  m = maximum c

exercise3 :: IO ()
exercise3 = do
  putStr "\n"
  putStrLn $ histogram [1, 1, 1, 5]
  putStrLn $ histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]

-- Main
main :: IO ()
main = do
  exercise1
  exercise2
  exercise3
