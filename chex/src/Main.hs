module Main where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

isPerson :: Either PersonInvalid Person -> Bool
isPerson (Right p) = True
isPerson _ = False

gimmePerson :: IO ()
gimmePerson = do
  putStr "Give a name: "
  name <- getLine
  putStr "Give an age: "
  age <- getLine
  if (isPerson (mkPerson name (read age))) then putStr "YAY!" else putStr "F" 
