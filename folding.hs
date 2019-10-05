myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = case f x of
                      True -> True
                      False -> myAny f xs

myElem :: Eq a => a -> [a] -> Bool
--myElem e lst = any (== e) lst
myElem e lst = foldr (\a b -> (a == e) || b) False lst

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []  

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = foldr (\a b -> (f a):b) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f lst = foldr (\a b -> if (f a) then  a:b else b )  [] lst

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = []
myMaximumBy f (x:xs) = foldr (\a b -> case (f a b) of {GT -> a; LT -> b}) xs (x:xs)
