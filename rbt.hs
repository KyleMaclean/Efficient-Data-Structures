-- Kyle Maclean
-- Red-Black Trees (RBTs)
-- November 2021

type Key = Double -- key median may require division of an odd number by 2
type Count = Int -- for storing the number of non-Leaf Nodes in a subtree
data Colour = Red | Black
  deriving (Eq,Show)
data RBT = Leaf | Node Colour RBT Count Key RBT Count
  deriving (Eq,Show)

key :: RBT -> Key
-- key Leaf = undefined
key (Node _ _ _ k _ _) = k

search :: Key -> RBT -> Bool
search k Leaf = False
search k (Node _ tl _ k' tr _)
 | k == k' = True
 | k < k' = search k tl
 | otherwise = search k tr

min' :: RBT -> Maybe Key
min' Leaf = Nothing
min' (Node _ Leaf _ k _ _) = Just k
min' (Node _ tl _ _ _ _) = min' tl

max' :: RBT -> Maybe Key
max' Leaf = Nothing
max' (Node _ _ _ k Leaf _) = Just k
max' (Node _ _ _ _ tr _) = max' tr

traverse' :: RBT -> [Key]
traverse' Leaf = []
traverse' (Node _ tl _ k tr _) = traverse' tl ++ k : traverse' tr

balance :: RBT -> RBT -- fixes a double red occurrence
balance (Node Black (Node Red (Node Red t1 c1 k1 t2 c2) _ k2 t3 c3) _ k3 t4 c4)
 = Node Red (Node Black t1 c1 k1 t2 c2) (c1+c2+1) k2 (Node Black t3 c3 k3 t4 c4) (c3+c4+1)
balance (Node Black (Node Red t1 c1 k1 (Node Red t2 c2 k2 t3 c3) _) _ k3 t4 c4)
 = Node Red (Node Black t1 c1 k1 t2 c2) (c1+c2+1) k2 (Node Black t3 c3 k3 t4 c4) (c3+c4+1)
balance (Node Black t1 c1 k1 (Node Red t2 c2 k2 (Node Red t3 c3 k3 t4 c4) _) _)
 = Node Red (Node Black t1 c1 k1 t2 c2) (c1+c2+1) k2 (Node Black t3 c3 k3 t4 c4) (c3+c4+1)
balance (Node Black t1 c1 k1 (Node Red (Node Red t2 c2 k2 t3 c3) _ k3 t4 c4) _)
 = Node Red (Node Black t1 c1 k1 t2 c2) (c1+c2+1) k2 (Node Black t3 c3 k3 t4 c4) (c3+c4+1)
balance t = t

blackRoot :: RBT -> RBT
blackRoot (Node Red t1 c1 k t2 c2) = (Node Black t1 c1 k t2 c2)
blackRoot t = t

redRoot :: RBT -> RBT
redRoot (Node Black t1 c1 k t2 c2) = Node Red t1 c1 k t2 c2
redRoot t = t

ins :: Key -> RBT -> RBT -- may yield weak RBT
ins k Leaf = Node Red Leaf 0 k Leaf 0 -- new elements are red
ins k t@(Node colour t1 c1 k' t2 c2)
 | k < k' = balance (Node colour (ins k t1) c1 k' t2 c2)
 | k > k' = balance (Node colour t1 c1 k' (ins k t2) c2)
 | otherwise = t -- if k is already present, then do nothing

del :: Key -> RBT -> RBT -- may yield weak RBT
del k Leaf = Leaf -- k not found
del k (Node _ t1 c1 k' t2 c2)
 | k < k' = delL k t1 c1 k' t2 c2
 | k > k' = delR k t1 c1 k' t2 c2
 | otherwise = fuse t1 c1 t2 c2

insert :: Key -> RBT -> RBT
insert k t = operate ins k t

delete :: Key -> RBT -> RBT
delete k t = operate del k t

operate :: (Key -> RBT -> RBT) -> Key -> RBT -> RBT -- yields strong RBT
operate f k t = recount k $ blackRoot $ f k t

recount :: Key -> RBT -> RBT -- counts how deep the key is/would be and distributes that number
recount k t = distribute (count 0 k t) k t

count :: Count -> Key -> RBT -> Count -- determines the depth of k relative to c
count c k Leaf = c - 1 -- only matches if k was deleted (go up one level from where k would have been)
count c k t@(Node _ tl _ k' tr _)
 | k < k' = count (c+1) k tl
 | k > k' = count (c+1) k tr
 | otherwise = c

distribute :: Count -> Key -> RBT -> RBT -- label current node with c, decrement c, recursively label children
distribute 0 _ t@(Node colour t1 _ k t2 _) = Node colour t1 0 k t2 0
distribute c k t@(Node colour tl cl k' tr cr)
 | k < k' = Node colour (distribute (c - 1) k tl) c k' tr cr
 | k > k' = Node colour tl cl k' (distribute (c - 1) k tr) c

delL :: Key -> RBT -> Count -> Key -> RBT -> Count -> RBT
delL k t1@(Node Black _ _ _ _ _) c1 k' t2 c2 = balL (del k t1) c1 k' t2 c2
delL k t1@(Node Red _ _ _ _ _) c1 k' t2 c2 = Node Red (del k t1) c1 k' t2 c2

balL :: RBT -> Count -> Key -> RBT -> Count -> RBT
balL (Node Red t1 c1 kl t2 c2) _ k t3 c3
 = Node Red (Node Black t1 c1 kl t2 c2) (c1+c2+1) k t3 c3
balL t1 c1 k (Node Black t2 c2 kr t3 c3) _
 = balance (Node Black t1 c1 k (Node Red t2 c2 kr t3 c3) (c2+c3+1))
balL t1 c1 k (Node Red (Node Black t2 c2 krl t3 c3) _ kr t4 c4) _
 = Node Red (Node Black t1 c1 k t2 c2) (c1+c2+1) krl (balance (Node Black t3 c3 kr (redRoot t4) c4)) (c3+c4+1)

delR :: Key -> RBT -> Count -> Key -> RBT -> Count -> RBT
delR k t1 c1 k' t2@(Node Black _ _ _ _ _) c2 = balR t1 c1 k' (del k t2) c2
delR k t1 c1 k' t2@(Node Red _ _ _ _ _) c2 = Node Red t1 c1 k' (del k t2) c2

balR :: RBT -> Count -> Key -> RBT -> Count -> RBT
balR t1 c1 k (Node Red t2 c2 kr t3 c3) _
 = Node Red t1 c1 k (Node Black t2 c2 kr t3 c3) (c2+c3+1)
balR (Node Black t1 c1 kl t2 c2) _ k t3 c3
 = balance (Node Black (Node Red t1 c1 kl t2 c2) (c1+c2+1) k t3 c3)
balR (Node Red t1 c1 kl (Node Black t2 c2 klr t3 c3) _) _ k t4 c4
 = Node Red (balance (Node Black (redRoot t1) c1 kl t2 c2)) (c1+c2+1) klr (Node Black t3 c3 k t4 c4) (c3+c4+1)

fuse :: RBT -> Count -> RBT -> Count -> RBT -- all elements of t1 must be < all elements of t2
fuse Leaf _ Leaf _ = Leaf
fuse Leaf _ t2 _ = t2
fuse t1 _ Leaf _ = t1
fuse t1@(Node Black _ _ _ _ _) c1 (Node Red t3 c3 y t4 c4) _
 = Node Red (fuse t1 c1 t3 c3) (c1+c3+1) y t4 c4
fuse (Node Red t1 c1 y t2 c2) _ t3@(Node Black _ _ _ _ _) c3
 = Node Red t1 c1 y (fuse t2 c2 t3 c3) (c2+c3)
fuse t1@(Node Red t3 c3 k t4 c4) c1 t2@(Node Red t5 c5 y t6 c6) c2
 = case t4t5 of
   s@(Node Red s1 cs1 z s2 cs2)
    -> Node Red (Node Red t3 c3 k s1 cs1) (c3+cs1+1) z (Node Red s2 cs2 y t6 c6) (cs2+c6+1)
   otherwise
    -> Node Red t3 c3 k (Node Red t4t5 (c4+c5) y t6 c6) (c4+c5+c6+1)
 where t4t5 = fuse t4 c4 t5 c5
fuse t1@(Node Black t3 c3 k t4 c4) c1 t2@(Node Black t5 c5 y t6 c6) c2
 = case t4t5 of
   s@(Node Red s1 cs1 z s2 cs2)
    -> Node Red (Node Black t3 c3 k s1 cs1) (c3+cs1+1) z (Node Black s2 cs2 y t6 c6) (cs2+c6+1)
   otherwise
    -> balL t3 c3 k (Node Black t4t5 (c4+c5) y t6 c6) (c3+c4+c5+c6+1)
 where t4t5 = fuse t4 c4 t5 c5

intersection :: RBT -> RBT -> [Key] -- traverse both trees in-order, then traverse and compare both resultant lists
intersection t1 t2 = int (traverse' t1) (traverse' t2)

int :: [Key] -> [Key] -> [Key] -- returns the sorted keys present in both sorted lists
int xs [] = [] -- early termination because ys is already exhausted
int [] ys = [] -- early termination because xs is already exhausted
int (x:xs) (y:ys)
 | x == y = x:(int xs ys)
 | x > y = int (x:xs) ys
 | otherwise = int xs (y:ys)

median :: RBT -> Key
median t@(Node _ _ cl _ _ cr)
 | n `mod` 2 == 0 = (t `at` i + t `at` j) / 2
 | otherwise = t `at` j
  where n = cl + cr + 1 -- number of non-Leaf Nodes in the root's children and the root itself
        i = n `div` 2
        j = i + 1

at :: RBT -> Count -> Key -- retrieves the key at the index in the list where it would be after an in-order traversal
t@(Node _ Leaf _ k' Leaf _) `at` _ = k'
t@(Node _ Leaf _ k' tr _) `at` i 
 | i == 1 = k'
 | otherwise = tr `at` (i - 1)
t@(Node _ tl cl k' Leaf _) `at` i
 | i == cl + 1 = k'
 | otherwise = tl `at` i
t@(Node _ tl cl k' tr cr) `at` i
 | i <= cl = tl `at` i
 | i == cl + 1 = k'
 | otherwise = tr `at` (i - cl - 1)

-- example trees for testing:
-- RBT data type for reference: Leaf | Node Colour RBT Count Key RBT Count
ta = Node Black (Node Red Leaf 0 8 Leaf 0) 1 16 (Node Red Leaf 0 32 Leaf 0) 1
tb = Node Red (Node Black Leaf 0 8 Leaf 0) 1 16 (Node Black Leaf 0 32 Leaf 0) 1
tc = Node Black (Node Black Leaf 0 8 Leaf 0) 1 16 (Node Black Leaf 0 32 Leaf 0) 1
tc9 = Node Black (Node Black Leaf 0 8.0 (Node Red Leaf 0 9.0 Leaf 0) 1) 2 16.0 (Node Black Leaf 0 32.0 Leaf 0) 1
td = Node Black (Node Black Leaf 0 8 Leaf 0) 1 16 (Node Black Leaf 0 32 (Node Red Leaf 0 64 Leaf 0) 1) 2
