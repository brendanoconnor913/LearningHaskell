data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust (Just k) = True

myIterate :: (a -> a) -> a -> [a]
myIterate f i = [i] ++ (myIterate f (f i))

getfst :: Maybe (a, b) -> a
getfst (Just (a, b)) = a

getsnd :: Maybe (a, b) -> b
getsnd (Just (a, b)) = b

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f i = if (isJust (f i)) then [(getfst (f i))] ++ (myUnfoldr f (getsnd (f i))) else [] 

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, (f a))) x

getl :: Maybe (a,b,a) -> a
getl (Just (x,y,z)) =  x

getr :: Maybe (a,b,a) -> a
getr (Just (x,y,z)) = z

getc :: Maybe (a,b,a) -> b
getc (Just (z,y,x)) = y

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = if (isJust (f x)) then Node (unfold f (getl (f x))) (getc(f x)) (unfold f (getr (f x))) else Leaf 

treeTake :: Integer -> BinaryTree a -> BinaryTree a
treeTake 0 _ = Leaf
treeTake _ (Leaf) = Leaf
treeTake n tree = go n 0 tree 
    where go n acc (Node lt ct rt) 
            | (n == acc) = Leaf
            | otherwise = (Node (go n (acc+1) (lt)) (ct) (go n (acc+1) (rt))) 
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = treeTake n (unfold (\a -> Just ((a+1), (a), (a+1))) 0)
