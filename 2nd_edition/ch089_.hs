--8.9.1
data Nat = Zero | Succ Nat
  deriving Show

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

one :: Nat
one = Succ Zero
mult :: Nat -> Nat -> Nat
mult Zero n = Zero
-- mult one n = n
mult (Succ n) m = add n (mult n m) {- somehow
*Main> multChannel 4 5
6  -- i don't understand -- i'll have to write down the evaluation when i get off this plane    -} 

addChannel :: Int -> Int -> Int
addChannel x y = nat2int (add (int2nat x) (int2nat y))
multChannel :: Int -> Int -> Int
multChannel x y = nat2int (mult (int2nat x) (int2nat y))

--8.9.2 -- redefine occurs for search trees instead of regular trees. 

data SearchTree a = Ord a => Leaf a | Node (SearchTree a) a (SearchTree a) deriving Show

occurs' :: Ord a => a -> Tree a -> Bool -- from the text
occurs' x (Leaf y)                 = x==y
occurs' x (Node l y r) | x==y      = True
                       | x <y      = occurs' x l
                       | otherwise = occurs' x r
-- redo this w compare :: Ord a => a->a->Ordering and explain why its better.

occurs :: Ord a => a -> SearchTree a -> Bool
occurs x (Leaf y)     = True
occurs x (Node l y r) = False

--8.9.3
data BinTree a = LeafBin a | NodeBin (BinTree a) (BinTree a) deriving Show

numLeaves :: BinTree a -> Int
numLeaves LeafBin x = nat2int one
numLeaves NodeBin t1 t2 = nat2int (add (numLeaves t1) (numLeaves t2)) -- i believe this is correct. 

balanced :: BinTree a -> Bool
balanced LeafBin x = True
balanced NodeBin t1 t2 | numLeaves t1 == numLeaves t2 = True
                       | otherwise = False -- i know, doesn't account for "off by one" for odd numbers, yet... 

