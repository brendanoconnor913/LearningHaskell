import Control.Monad 

bind :: Monad m => (a -> m b) -> m a -> m b
bind f s = join (fmap f s) 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

--mkSphericalCow :: String -> Int -> Int -> Maybe Cow
--mkSphericalCow name' age' weight' =
--  case noEmpty name' of
--    Nothing -> Nothing
--    Just nammy ->
--      case noNegative age' of
--        Nothing -> Nothing
--        Just agey ->
--          case noNegative weight' of
--            Nothing -> Nothing
--            Just weighty ->
--              weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weight)

helper :: Monad m => [a] -> (a -> m b) -> [b]
helper [] _ = []
helper (x:xs) f = ((f x) >>= id):(helper xs f) 

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = []
meh (x:xs) f = pure (helper (x:xs) f) 
