azip :: [a] -> [b] -> [(a,b)]
--azip [] _ = []
--azip _ [] = []
--azip (x:xs) (y:ys) = (x,y):azip xs ys
azip x y = azipWith (\x y -> (x, y)) x y 
azipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
azipWith _ [] _ = []
azipWith _ _ [] = []
azipWith f (x:xs) (y:ys) = f x y:(azipWith f xs ys)
