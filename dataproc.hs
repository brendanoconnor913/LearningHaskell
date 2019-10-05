import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    ]

--filterDbDate :: [DatabaseItem] -> [UTCTime]
--filterDbDate [] = []
--filterDbDate (x:xs) = case x of (DbDate date) -> date : filterDbDate xs 
--                                otherwise -> filterDbDate xs

--filterDbDate :: [DatabaseItem] -> [UTCTime]
--filterDbDate db = 
--    foldr maybecons []
--    where maybecons a b = 
--            case a of
--              (DbDate date) -> date : b
--              _ -> b

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr summer 0 db
    where summer a b = 
            case a of
              (DbNumber int) -> int + b
              _              -> b
            
