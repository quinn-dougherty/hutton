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
mult (Succ n) m = add m (mult n m) {- this suddenly worked. Don't fully see how all my previous attempts weren't working.   -} 

addChannel :: Int -> Int -> Int
addChannel x y = nat2int (add (int2nat x) (int2nat y))
multChannel :: Int -> Int -> Int
multChannel x y = nat2int (mult (int2nat x) (int2nat y))

--8.9.2 -- redefine occurs for search trees instead of regular trees. 

data SearchTree a = Leaf a | Node (SearchTree a) a (SearchTree a) deriving Show -- tried to do this w "Ord a => " but it said needed to be existentially quantified (not a forall) and something about GADTs 

occurs' :: Ord a => a -> SearchTree a -> Bool -- from the text
occurs' x (Leaf y)                 = x==y
occurs' x (Node l y r) | x==y      = True
                       | x <y      = occurs' x l
                       | otherwise = occurs' x r
-- redo this w compare :: Ord a => a->a->Ordering and explain why its better.

occurs :: Ord a => a -> SearchTree a -> Bool
occurs x (Leaf y)     = x==y
occurs x (Node l y r) | compare x y == LT = occurs x l
                      | compare x y == EQ = True
                      | compare x y == GT = occurs x r
-- this should be right. 

-- same trash data from file ch08.hs
s :: SearchTree Int
s = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
-- the only reason it would be better would be if the typechecker trashed some info after it verified correctness..? 


--8.9.3
data BinTree a = LeafBin a | NodeBin (BinTree a) (BinTree a) deriving Show

numLeaves :: BinTree a -> Int
numLeaves (LeafBin x) = nat2int one
numLeaves (NodeBin t1 t2) = ((+) (numLeaves t1) (numLeaves t2)) -- i believe this is correct. 

balanced :: BinTree a -> Bool
balanced (LeafBin x)                                    = True -- singleton or empty trees are balanced
balanced (NodeBin t1 t2) | numLeaves t1     == numLeaves t2 = True
                         | 1 + numLeaves t1 == numLeaves t2 = True
                         | numLeaves t1 - 1 == numLeaves t2 = True
                         | otherwise                        = False -- 

height :: BinTree a -> Int
height (LeafBin x) = 1
height (NodeBin t1 t2) = 1 + (max (height t1) (height t2)) -- i think this is correct. 

--trash data to test this
t :: BinTree Int
t = NodeBin (NodeBin (NodeBin (LeafBin 1) (LeafBin 0)) (LeafBin 2)) (NodeBin (LeafBin 1) (LeafBin 3)) -- this got a numLeaves 5, currently height 4, and not balanced. and balanced True (its the odd case, the last True case before otherwise). 

-- 8.9.4
balance :: [a] -> BinTree a
balance [x] = LeafBin x
balance xs  = NodeBin (balance (take n xs)) (balance (drop n xs))
  where n = div (length xs) 2

--8.9.5


--copying the abstract machine from 8.7, after having added Mult.
-- we'll just leave Mult at Mult and change Add to g, change Val to f... 
data Expr =
    Val Int
  | Add Expr Expr
  | Mult Expr Expr
  | UnOp Int
  | BinOp Expr Expr
          deriving Show

value :: Expr -> Int
value (Val n)    = n
value (Add x y)  = value x + value y
value (Mult x y) = value x * value y -- this evaluates left right. 

-- control stacks, for order of operations
type Cont = [Op]
data Op = EVAL1 Expr | ADD Int | EVAL2 Expr | MULT Int {-| BINOP Int | EVAL Expr | UNOP Int -} deriving Show
-- the all caps version is just what it looks like inside a List

-- making it EVAL1 and EVAL2 is pretty good for adding Multiplication, but i don't think its the most correct or the prettiest. 

eval' :: Expr -> Cont -> Int
eval' (Val n)    c = exec c n
eval' (Add x y)  c = eval' x (EVAL1 y : c)
eval' (Mult x y) c = eval' x (EVAL2 y : c)

exec :: Cont -> Int -> Int
exec []            n = n
exec (EVAL1 y : c) n = eval' y (ADD n : c)
exec (ADD n : c)   m = exec c (n+m)
exec (EVAL2 y : c) n = eval' y (MULT n : c)
exec (MULT n : c)  m = exec c (n*m)

--exec (EVAL y : c) n | 

value' :: Expr -> Int
value' e = eval' e []
-- that's the extent of copying.
-- now lets start back from Cont.
type BINOP = Int -> Int -> Int
type UNOP  = Int -> Int
data OpFld = EVAL Expr | BINOP Int | UNOP Int
type ContFld = [OpFld]

--dummy data
ex :: Expr
ex = Add (Mult (Val 3) (Val 4)) (Val 7)

evalFld :: Expr -> ContFld -> Int
evalFld (Val n)    c = exec (map UNOP c) n
evalFld (Add x y)  c = evalFld x (BINOP y : c)
execFld :: ContFld -> Int -> Int
execFld [] n = n
execFld (EVAL y : c) n = evalFld y (BINOP n : c)
{-
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
{- s.t. folde f g replaces each Val constructor in an expr by the function f and each add constructor by the function g -}
folde f g = 
convoluted and problematic my dude-} 

--8.9.6




--8.9.7
{-
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = Nothing
  Nothing == Just a  = Nothing
  Just a  == Just a  = Just a
-- is this... ?

instance Eq a => Eq [a] where
  xs == xs = xs
  -- gotta read about this

-}

--8.9.8

  -- see ch08.hs, we're going to edit/expand the book walkthru i already typed up. 
-- ****OK. So now, extend isTaut to support disjunction and equivalence. 

-- 8.9.9
-- -- in ch08.hs 
