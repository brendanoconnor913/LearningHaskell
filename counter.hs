
counter :: Eq a => [a] -> Int
counter l = go l 0
    where go l count
            | l == [] = count
            | otherwise = go (drop 1 l) (count+1)
