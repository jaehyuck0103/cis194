{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
    | x < 1 = []
    | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x < 1 = []
    | otherwise = (x `mod` 10) : toDigitsRev(x `div` 10)

toDigitsRevV2 :: Integer -> [Integer]
toDigitsRevV2 = reverse .toDigits

exercise1 :: IO ()
exercise1 = do
  print $ toDigits 1234 == [1,2,3,4]
  print $ toDigitsRev 1234 == [4,3,2,1]
  print $ toDigitsRevV2 1234 == [4,3,2,1]
  print $ toDigits 0 == []
  print $ toDigits (-17) == []

-- Exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:zs) = x : 2*y : doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

exercise2 :: IO ()
exercise2 = do
  print $ doubleEveryOther [8,7,6,5] == [16,7,12,5] 
  print $ doubleEveryOther [1,2,3] == [1,4,3]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = (sum $ toDigits x) + sumDigits ys

exercise3 :: IO ()
exercise3 = print $ sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5

-- Exercise 4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther $ toDigits x) `mod` 10 == 0

exercise4 :: IO ()
exercise4 = do
  print $ validate 4012888888881881 == True
  print $ validate 4012888888881882 == False

-- Main
main :: IO ()
main = do
    exercise1
    exercise2
    exercise3
    exercise4