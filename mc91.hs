mc91 :: (Num a, Ord a) => a -> a

mc91 n 
    | n > 100 = (n - 10)
    | otherwise = 91
