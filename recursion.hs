module Summation where

summation :: (Eq a, Num a) => a -> a

summation n = go n 0 
  where go n sum
          | n == 0 = sum
          | otherwise = go (n - 1) (sum + n) 
