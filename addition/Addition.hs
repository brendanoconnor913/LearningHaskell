module Addition where

import Test.Hspec
import Test.QuickCheck

summation :: (Eq a, Num a) => a -> a
summation n = go n 0 
  where go n sum
          | n == 0 = sum
          | otherwise = go (n - 1) (sum + n) 

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5,0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)
    it "summation of 0 .. 5 is 15" $ do
      summation 5 `shouldBe` 15
    it "summation of 0 .. 20 is 210" $ do
      summation 20 `shouldBe` 210
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

