{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1 (Slow)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

exercise1 :: IO ()
exercise1 = print $ take 35 fibs1

-- Exercise 2 (Fast)
fibs2_sub :: (Integer, Integer) -> (Integer, Integer)
fibs2_sub (x, y) = (y, x + y)

fibs2 :: [Integer]
fibs2 = map fst (iterate fibs2_sub (0, 1))

exercise2 :: IO ()
exercise2 = print $ take 35 fibs2

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show x = unwords $ take 20 (map show (streamToList x))

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x1 s1) = Cons (f x1) (streamMap f s1)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

startRuler :: Integer -> Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y + 1))

ruler :: Stream Integer
ruler = startRuler 0

exercise5 :: IO ()
exercise5 = do
  print nats
  print ruler

-- Exercise 6
xx :: Stream Integer
xx = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (* (-1))
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons a0 a') b@(Cons b0 b') =
    Cons (a0 * b0) (fromInteger a0 * b' + a' * b)

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = xx / Cons 1 (Cons (-1) (Cons (-1) (streamRepeat 0)))

exercise6 :: IO ()
exercise6 = do
  print $ xx ^ (4 :: Integer)
  print $ (1 + xx) ^ (5 :: Integer)
  print $ (xx ^ (2 :: Integer) + xx + 3) * (xx - 5)
  print fibs3

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix
    (a11 * b11 + a12 * b21)
    (a11 * b12 + a12 * b22)
    (a21 * b11 + a22 * b21)
    (a21 * b12 + a22 * b22)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = getA12 (Matrix 1 1 1 0 ^ n) where getA12 (Matrix _ a12 _ _) = a12

exercise7 :: IO ()
exercise7 = do
  print $ fibs4 0 == 0
  print $ fibs4 10 == 55


-- Main
main :: IO ()
main = do
  exercise1
  exercise2
  exercise5
  exercise6
  exercise7
