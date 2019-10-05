
counts = [(22,1),(14,1),(14,1),(99,1),(34,1)]
incrm :: [(Integer,Integer)] -> [(Integer,Integer)]
incrm (x:xs) = (fst x, (snd x)+1):xs
tot = foldr (\a b -> if (fst a == fst (head b)) then (incrm b) else a:b) [(0,0)] counts 
