
toList :: String -> [String]
toList [] = []
toList (' ':xs) = toList xs
toList xs = 
    takeWhile(/= ' ') xs:toList (dropWhile (/= ' ') xs)

--myFilter :: String -> [String]
myFilter line = 
    filter (\x -> not (elem x ["the", "a", "an"])) words 
    where words = toList line

main = print (toList "this is a test")
