import Data.Monoid
import Test.QuickCheck
import Control.Monad


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada 
  mappend (Only k) Nada = (Only k)
  mappend Nada (Only k) = (Only k)
  mappend Nada Nada = Nada
  mappend (Only k) (Only l) = Only (k `mappend` l)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada 
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend (First' (Only x)) (First' Nada) = First' (Only x)
  mappend (First' (Only x)) (First' (Only y)) = First' (Only x) 

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = 
     First' String
  -> First' String
  -> First' String
  -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)


