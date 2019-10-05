data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

bmap :: (Ord a, Ord b) => (a -> b) -> BinaryTree a -> BinaryTree b
bmap _ Leaf = Leaf
bmap f (Node left a right) = Node (bmap f left) (f a) (bmap f right) 

testTree' :: BinaryTree Integer
testTree' = 
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if bmap (+1) testTree' == mapExpected
  then print "I WIN"
  else error "test failed!"

treeToList :: BinaryTree a -> [a]
treeToList Leaf = [] 
treeToList (Node Leaf a right) = (a:[]) ++ (treeToList right)
treeToList (Node left a right) = (treeToList left ++ (a:[])) ++ (treeToList right) 

listExpected = [3,1,4]

listOkay =
  if treeToList testTree' == listExpected
  then print "List is correct"
  else error "list function failed"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f lp left 
    where lp = f a (foldTree f b right) 

--mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b 
--mapTree' f bt = foldTree (\a b -> Node ) Leaf bt 
