f :: Either a b -> [a] -> [a] 
f (Left x) [] = [x]
f (Left x) lst = lst ++ [x]
f _ lst = lst

lefts' :: [Either a b] -> [a]
lefts' = foldr f []

j :: Either a b -> [b] -> [b]
j (Right x) [] = [x]
j (Right x) lst = lst ++ [x]
j _ lst = lst

rights' :: [Either a b] -> [b]
rights' = foldr j []

p :: Either a b -> ([a], [b]) -> ([a], [b])
p (Left x) (lsta, lstb) = ((lsta ++ [x]), lstb)
p (Right x) (lsta, lstb) = (lsta, (lstb ++ [x]))

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr p ([],[])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left x) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = (f x)
either' _ g (Right x) = (g x)

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left x) = Nothing
eitherMaybe'' f (Right x) = Just (f x)
