import Data.List (elemIndex)
import Data.Monoid (Monoid)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

--y :: Maybe Integer
--y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

--tupled :: Maybe (Integer, Integer)
--tupled = (,) <$> y <*> z

--x :: Maybe Int
--x = elemIndex 3 [1, 2, 3, 4, 5]
--
--y :: Maybe Int
--y = elemIndex 4 [1, 2, 3, 4, 5]
--
--max' :: Int -> Int -> Int
--max' = max
--
--maxed :: Maybe Int
--maxed = (max' <$> x) <*> y
--
xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y 

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity y = Identity (f y) 

newtype Constant a b =
  Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x 

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant {getConstant = mempty}
  x <*> y = Constant (getConstant x `mappend` getConstant y) 
    
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil 
take' n Nil = Nil 
take' n (Cons x xs) = Cons x (take' (n-1) xs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

append :: List a -> List a -> List a
append Nil ys = ys
append ys Nil = ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f xs) (Cons a mxs) = append (Cons (f a) (fmap f mxs)) (xs <*> (Cons a mxs)) 
 
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = fold (\a b -> append (f a) b) Nil as

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

altf :: List (a -> b) -> List a -> List b
altf Nil _ = Nil
altf _ Nil = Nil
altf (Cons f xs) (Cons a mxs) = Cons (f a) (altf xs mxs)

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil) 
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' x) (ZipList' y) = ZipList' (altf x y) 

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x) 
  fmap f (First x) = First x

instance Applicative (Sum a) where
  pure = Second 
  (<*>) (First x) (First y) = First y
  (<*>) (First x) (Second y) = First x 
  (<*>) (Second x) (First y) = First y
  (<*>) (Second x) (Second y) = Second (x y) 

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success (f a) 

instance Monoid e => Applicative (Validation e) where
  pure = Success 
  (<*>) (Error e) (Success a) = Error e
  (<*>) (Success a) (Error e) = Error e
  (<*>) (Success f) (Success a) = Success (f a)
  (<*>) (Error e) (Error e2) = Error (e `mappend` e2) 

