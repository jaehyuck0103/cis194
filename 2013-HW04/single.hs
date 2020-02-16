{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs) | even x    = (x - 2) * fun1 xs
              | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate
  (\n -> if even n then n `div` 2 else 3 * n + 1)

exercise1 :: IO ()
exercise1 = do
  print $ fun1 [6, 18, 4] == fun1' [6, 18, 4]
  print $ fun2 10 == fun2' 10

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

treeLevel :: Tree a -> Integer
treeLevel Leaf           = -1
treeLevel (Node n _ _ _) = n

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node h left v right)
  | leftH > rightH = Node h left v newRight
  | leftH < rightH = Node h newLeft v right
  | otherwise      = Node (treeLevel newRight + 1) left v newRight
 where
  leftH    = treeLevel left
  rightH   = treeLevel right
  newLeft  = treeInsert x left
  newRight = treeInsert x right

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left _ right) =
  isBalanced left
    && isBalanced right
    && abs (treeLevel left - treeLevel right)
    <= 1

exercise2 :: IO ()
exercise2 = print $ isBalanced $ foldTree "ABCDEFGHIJ"

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\x y -> (not x && y) || (x && not y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr ((:) . f) []  -- (\x xs -> (:) (f x) xs) => ((:) .f)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

exercise3 :: IO ()
exercise3 = do
  print $ xor [False, True, False] == True
  print $ xor [False, True, False, False, True] == False
  print $ map (* 2) ([1 .. 5] :: [Integer]) == map' (* 2) [1 .. 5]
  print $ map (* 2) ([1 .. 5] :: [Integer]) == map'' (* 2) [1 .. 5]
  print $ foldl (-) 0 ([1 .. 10] :: [Integer]) == myFoldl (-) 0 [1 .. 10]

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [ (x, y) | x <- xs, y <- ys ]

excludeList :: Integer -> [Integer]
excludeList n = filter
  (<= n)
  (map (\(i, j) -> i + j + 2 * i * j)
       (filter (uncurry (<=)) (cartProd [1 .. n] [1 .. n]))
  )

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2 * x + 1) (filter (`notElem` excludeList n) [1 .. n])

exercise4 :: IO ()
exercise4 = print $ sieveSundaram 100

-- Main
main :: IO ()
main = do
  exercise1
  exercise2
  exercise3
  exercise4
