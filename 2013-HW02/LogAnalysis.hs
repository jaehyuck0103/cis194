{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where

import           Log

-- Exercise 1
wordsToLogMessage :: [String] -> LogMessage
wordsToLogMessage ("I" : time : message) =
  LogMessage Info (read time) (unwords message)
wordsToLogMessage ("W" : time : message) =
  LogMessage Warning (read time) (unwords message)
wordsToLogMessage ("E" : severity : time : message) =
  LogMessage (Error (read severity)) (read time) (unwords message)
wordsToLogMessage message = Unknown (unwords message)

parseMessage :: String -> LogMessage
parseMessage = wordsToLogMessage . words

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

exercise1 :: IO ()
exercise1 = do
  print $ parseMessage "E 2 562 help help" == LogMessage (Error 2)
                                                         562
                                                         "help help"
  print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
  print $ parseMessage "This is not in the right format" == Unknown
    "This is not in the right format"

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newLog      Leaf = Node Leaf newLog Leaf
insert newLog@(LogMessage _ newTime _) (Node treeL nodeLog@(LogMessage _ nodeTime _) treeR)
  | newTime < nodeTime
  = Node (insert newLog treeL) nodeLog treeR
  | otherwise
  = Node treeL nodeLog (insert newLog treeR)
insert _ (Node _ (Unknown _) _) = Leaf   -- 애초에 insert에서 Unknown을 무시해서 이런 경우가 없긴한데, 모든 패턴을 소화하기 위해 넣어둠.

-- Exercise 3
build :: [LogMessage] -> MessageTree
build []       = Leaf
build (x : xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node treeL nodeMessage treeR) =
  inOrder treeL ++ [nodeMessage] ++ inOrder treeR

-- Exercise 5
takeSevere :: [LogMessage] -> [String]
takeSevere (LogMessage (Error severity) _ message : xs) | severity >= 50 =
  message : takeSevere xs
takeSevere (_ : xs) = takeSevere xs
takeSevere []       = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = takeSevere . inOrder . build

-- Main
main :: IO ()
main = do
  exercise1
  print =<< testParse parse 10 "error.log"
  print =<< testWhatWentWrong parse whatWentWrong "sample.log"
