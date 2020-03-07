{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Employee
import           Data.Tree
import           Data.List

-- Exercise 1
instance Semigroup GuestList where
  (<>) (GL l1 fun1) (GL l2 fun2) = GL (l1 ++ l2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list funsum) = GL (emp : list) (empFun emp + funsum)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root children) = f root (map (treeFold f) children)

exercise2 :: IO ()
exercise2 = print $ treeFold (\x xs -> glCons x $ mconcat xs) testCompany

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel nextBoss sublists = (withBoss, withoutBoss)
 where
  withoutBoss = foldMap (uncurry moreFun) sublists
  withBoss    = glCons nextBoss $ foldMap snd sublists

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel tree

exercise4 :: IO ()
exercise4 = print $ maxFun testCompany

-- Exercise 5
formatGL :: GuestList -> String
formatGL (GL lst fun) =
  "Total fun: " ++ show fun ++ "\n" ++ unlines (sort (empName <$> lst))

exercise5 :: IO ()
exercise5 = readFile "company.txt" >>= computeGuestList >>= putStrLn
  where computeGuestList = return . formatGL . maxFun . read

-- Main
main :: IO ()
main = do
  exercise2
  exercise4
  exercise5
