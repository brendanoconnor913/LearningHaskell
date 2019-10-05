import Data.List (intersperse)

replaceThe :: String -> String
replaceThe str = go (words str) "" 
    where go line final
            | (line == []) = (take ((length final)-1) final) 
            | ((head line) == "the") = go (tail line) (final ++ "a ")
            | otherwise = go (tail line) (final ++ ((head line) ++ " "))  

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel str = go (words str) 0 
    where go line acc 
            | (length line == 1) = acc 
            | (((head line) == "the") && (isVowel(head (line !! 1)))) = go (tail line) (acc + 1)
            | otherwise = go (tail line) (acc)  

countVowels :: String -> Integer
countVowels s = foldr (\x y -> (foldr (\a b -> if (isVowel a) then (b+1) else b) 0 x) + y) 0 (words s)

countCons ::String -> Integer
countCons s = foldr (\x y -> (foldr (\a b -> if (not (isVowel a)) then (b+1) else b) 0 x) + y) 0 (words s)

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if (countVowels s) >= (countCons s) then Nothing else (Just (Word' s))
 
