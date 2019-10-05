data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x 
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y) 

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)  

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x 
  (<*>) (Two f g) (Two x y) = Two x (g y) 


