enumFromToN :: (Enum a, Eq a, Num a) => a -> a -> [a]

enumFromToN sa sp = go sa sp []
    where go sa sp lst
            | sa == sp  = lst ++ sa:[] 
            | otherwise = go (sa+1) sp (lst ++ sa:[]) 
