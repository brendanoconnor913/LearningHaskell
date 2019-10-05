module Jammin where

data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show, Ord)

data JamJars = 
    JamJars { fruitname :: Fruit
          , numjars :: Int } 
          deriving (Eq, Show, Ord)


