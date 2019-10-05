isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust (Just k) = True

isNothing :: Maybe a -> Bool
isNothing (Nothing) = True
isNothing (Just k) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f (Just a) = (f a)
mayybee z _ (Nothing) = z

fromMaybe :: a -> Maybe a -> a
fromMaybe b Nothing = b
fromMaybe b (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):xs) = a : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs 

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (x:xs) = f x (flipMaybe xs)  

f :: Maybe a -> Maybe [a] -> Maybe [a]
f Nothing _ = Nothing
f _ Nothing = Nothing
f (Just x) (Just []) = Just [x]
f (Just x) (Just lst) = Just (lst ++ [x]) 
