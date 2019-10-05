import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf all@(x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf all ys 

wordHelper :: String -> (String, String)
wordHelper wd@(x:xs) = ((toUpper x) : xs, wd)
stringHelper :: [String] -> [(String,String)]
stringHelper [] = []
stringHelper (x:xs) = (wordHelper x) : stringHelper xs
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = stringHelper (words s) 
