
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = if elem == x then True else myElem elem xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 elem (x:xs) = myAny (\x -> elem == x) (x:xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x:[]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) =  x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain (x:xs) = squishMap id (x:xs) 

-- This isn't working perfectly may be worth coming back to
--myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
--myMaximumBy f (x:xs) = go f xs x
--    where go _ [] acc = acc
--          go g (y:ys) acc = case g y acc of
--                                 GT -> go g ys y
--                                 _ -> go g ys acc
--

