{-# LANGUAGE FlexibleInstances #-}

newtype Identity a = Identity a
data Pair a = Pair a a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four' a b = Four' a a a b

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Functor (Three' a) where
  fmap f (Three' a b p) = Three' a (f b) (f p)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

instance Functor (Four' a) where
  fmap f (Four' l m n o) = Four' l m n (f o)

data Possibly a = 
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ (LolNope) = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

--data Sum a b =
--    First a
--  | Second b
--  deriving (Eq, Show)
--
--instance Functor (Sum a) where
--  fmap _ (First a) = First a
--  fmap f (Second b) = Second (f b)

data Sum b a = 
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second e) = Second e

data Company a b c =
    DeepBlue a b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a 

data K a b =
  K a

instance Functor (K a) where
  fmap f (K a) = K a 

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x)) 

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x) 

data Parappa f g a = 
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap p (DaWrappa x y) = DaWrappa (fmap p x) (fmap p y) 

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap p (IgnoringSomething r x) = IgnoringSomething r (fmap p x)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z) 

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f (Cons a x) = Cons (f a) (fmap f x)
  fmap f Nil = Nil

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (NoGoat) = NoGoat

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print x y) = Print x (f y) 
  fmap f (Read x) = Read (f . x)

