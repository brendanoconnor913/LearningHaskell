perm [] _ = []
perm (x:xs) list2 = map (\y -> (x,y)) list2 : perm xs list2 

fullperm [] [] = []
fullperm (x:xs) (y:ys) = fmap (\z -> (x,z)) (y:ys) : fullperm xs (y:ys) 

full [] [] = []
full _ [] = []
full [] _ = []
full (x:xs) ((y,z):yzs) = map (\p -> (fst p,snd p,x)) ((y,z):yzs) : full xs ((y,z):yzs)

final l1 l2 = concat(full l1 (concat(perm l1 l2)))
