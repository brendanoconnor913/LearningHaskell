{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany (Int, String) where
    tooMany s = fst s > 100

instance TooMany (Int, Int) where
    tooMany s = (fst s + snd s) > 20

instance TooMany (Num a => (a, a)) where
    tooMany s = (fst s + snd s) > 10 
