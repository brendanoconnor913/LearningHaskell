module Cipher where
import Data.Char

charToNum :: String -> [Int]
charToNum "" = [] 
charToNum (x:xs) = if x /= ' ' then ord x:charToNum xs else (-1):charToNum xs 

shifter :: [Int] -> Int -> [Int]
shifter [] _ = []
shifter (x:xs) s = if (x /= -1) then safeadd x s:shifter xs s else x:shifter xs s 

safeadd :: Int -> Int -> Int
safeadd n s = 
    if isUpper . chr $ n then (mod (n+s-65) 26)+65 else (mod (n+s-97) 26)+97

numToChar :: [Int] -> [Char]
numToChar [] = []
numToChar (x:xs) = if x == -1 then ' ':numToChar xs else chr x:numToChar xs

caesarCipher :: String -> Int -> String
caesarCipher "" _ = ""
caesarCipher line s = numToChar(shifter(charToNum line) s)

unCaesar :: String -> Int -> String
unCaesar "" _ = ""
unCaesar line a = numToChar(shifter(charToNum line) (26 - (mod a 26)))
