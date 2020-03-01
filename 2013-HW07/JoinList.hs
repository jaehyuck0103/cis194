{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
import           Sized
import           Scrabble
import           Buffer
import           Editor

-- Pre Given Utils
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a    ) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ n (Append m x y) | n < 0         = Nothing
                        | n < left_size = indexJ n x
                        | n < root_size = indexJ (n - left_size) y
                        | otherwise     = Nothing
 where
  left_size = Sized.getSize $ Sized.size $ tag x
  root_size = Sized.getSize $ Sized.size m

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n sc@(Single _ _) | n <= 0    = sc
                        | otherwise = Empty
dropJ n sc@(Append m x y) | n <= 0        = sc
                          | n < left_size = dropJ n x +++ y
                          | n < root_size = dropJ (n - left_size) y
                          | otherwise     = Empty
 where
  left_size = Sized.getSize $ Sized.size $ tag x
  root_size = Sized.getSize $ Sized.size m

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n sc@(Single _ _) | n <= 0    = Empty
                        | otherwise = sc
takeJ n sc@(Append m x y) | n <= 0        = Empty
                          | n < left_size = takeJ n x
                          | n < root_size = x +++ takeJ (n - left_size) y
                          | otherwise     = sc
 where
  left_size = Sized.getSize $ Sized.size $ tag x
  root_size = Sized.getSize $ Sized.size m

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (Scrabble.scoreString str) str

exercise3 :: IO ()
exercise3 = print $ scoreLine "yay " +++ scoreLine "haskell!"

-- Exercise 4
instance Monoid m => Semigroup (JoinList m a) where
  (<>) = (+++)
instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString   = unlines . jlToList
  fromString = mconcat . map enlist . lines
  line       = indexJ
  replaceLine n str buf = case indexJ n buf of
    Nothing -> buf
    Just _ ->
      takeJ n buf
        +++ Single (Scrabble.scoreString str, Size 1) str
        +++ dropJ (n + 1) buf
  numLines = getSize . size . tag
  value    = getScore . fst . tag

enlist :: String -> JoinList (Score, Size) String
enlist str = Single (Scrabble.scoreString str, Size 1) str

newbuf :: JoinList (Score, Size) String
newbuf = (fromString . unlines)
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ]

main :: IO ()
main = do
  exercise3
  runEditor editor newbuf
