module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
    | n == 9 = "nine"
    | n == 8 = "eight"
    | n == 7 = "seven"
    | n == 6 = "six"
    | n == 5 = "five"
    | n == 4 = "four"
    | n == 3 = "three"
    | n == 2 = "two"
    | n == 1 = "one"
    | otherwise = error "invalid"

digits :: Int -> [Int]
digits n = go n []
    where go n list 
            | n == 0 = list 
            | otherwise = go (div n 10) ([(mod n 10)] ++ list)

wordNumber :: Int -> String 
wordNumber n = concat x
    where x = intersperse "-" . map digitToWord . digits $ n
