multiply :: (Integral a) => a -> a -> a
multiply n1 n2 = go n1 n2 0 0
    where go n1 n2 total num
            | num == n2 = total
            | otherwise = go n1 n2 (total + n1) (num + 1) 
