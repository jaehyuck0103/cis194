{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
import           ExprT
import           Parser
import           StackVM
import qualified Data.Map                      as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit x    ) = x
eval (ExprT.Add x1 x2) = eval x1 + eval x2
eval (ExprT.Mul x1 x2) = eval x1 * eval x2

exercise1 :: IO ()
exercise1 =
  print
    $  eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4))
    == 20

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
  Nothing   -> Nothing
  Just expr -> Just (eval expr)

exercise2 :: IO ()
exercise2 = do
  print $ evalStr "(2+3)*4" == Just 20
  print $ evalStr "2+3*4" == Just 14
  print $ evalStr "2+3*" == Nothing

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- 단순히 "mul (add (lit 2) (lit 3)) (lit 4)" 이라고만 표현하면, 
-- Expr type class에 속한 건 알 수 있지만, 그 안의 어떤 instace type인지는 명확하지 않다.
-- 아래와 같은 방법 이용해서 명확하게 할 수 있다.
exercise3 :: IO ()
exercise3 = do
  print $ (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == ExprT.Mul
    (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3))
    (ExprT.Lit 4)
  print $ reify (mul (add (lit 2) (lit 3)) (lit 4)) == ExprT.Mul
    (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3))
    (ExprT.Lit 4)

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit 0 = False
  lit _ = True
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x1) (MinMax x2) = MinMax (max x1 x2)
  mul (MinMax x1) (MinMax x2) = MinMax (min x1 x2)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x1) (Mod7 x2) = Mod7 ((x1 + x2) `mod` 7)
  mul (Mod7 x1) (Mod7 x2) = Mod7 ((x1 * x2) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

exercise4 :: IO ()
exercise4 = do
  print $ (testExp :: Maybe Integer) == Just (-7)
  print $ (testExp :: Maybe Bool) == Just True
  print $ (testExp :: Maybe MinMax) == Just (MinMax 5)
  print $ (testExp :: Maybe Mod7) == Just (Mod7 0)

-- Exercise 5
instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

exercise5 :: IO ()
exercise5 = print $ compile "(3 * -4) + 5"

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VVar String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
    deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a _ = Just a
  add a b vs = (+) <$> a vs <*> b vs
  mul a b vs = (*) <$> a vs <*> b vs

withVars
  :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

exercise6 :: IO ()
exercise6 = do
  print $ withVars [("x", 6)] (add (lit 3) (var "x")) == Just 9
  print $ withVars [("x", 6)] (add (lit 3) (var "y")) == Nothing
  print
    $  withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x")))
    == Just 54

-- Main
main :: IO ()
main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
  exercise6
