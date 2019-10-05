module EitherMonad where
import Control.Monad

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded       :: Founded
    , programmers   :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears  Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0   = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0   = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded   <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second x) = Second (f x) 

instance Applicative (Sum a) where
  pure = Second 
  (<*>) (First x) _ = (First x)
  (<*>) _ (First x) = (First x)
  (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = (First x)
  (>>=) (Second x) f = f x 

data Nope a =
  NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure 
  (>>=) NopeDotJpg _ = NopeDotJpg

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x xs) y = Cons x (xs `mappend` y)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append ys Nil = ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f xs) (Cons a mxs) = append (Cons (f a) (fmap f mxs)) (xs <*> (Cons a mxs)) 
 
instance Monad List where
  return x = Cons x Nil
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = join $ fmap f (Cons x xs) 

j :: Monad m => m (m a) -> m a
j mn = mn >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f mn = fmap f mn

pull :: [m b] -> m [b]
pull [] = []
pull (x:xs) = 

