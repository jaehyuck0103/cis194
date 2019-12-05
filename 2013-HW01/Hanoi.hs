{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src goal _ = [(src, goal)]
hanoi n src goal tmp
    | n < 1 = []
    | otherwise = hanoi (n-1) src tmp goal ++ [(src, goal)] ++ hanoi (n-1) tmp goal src


-- Main
main :: IO()
main = do
    print $ hanoi 2 "a" "b" "c"
    print $ hanoi 3 "a" "b" "c"
    print (length (hanoi 15 "a" "b" "c") == 32767)