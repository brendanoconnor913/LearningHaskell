data Nat = 
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat i = go i Zero
    where go num nat
            | num < 0 = Nothing
            | num == 0 = (Just nat)
            | otherwise = go (num - 1) (Succ nat)
