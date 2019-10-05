foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
        True    -> x
        False   -> y

foldBoolg :: a -> a -> Bool -> a
foldBoolg x y b 
    | b         = x 
    | otherwise = y  
