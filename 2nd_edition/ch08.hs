-- 8.1 Type declarations

type String' = [Char]
type Pos = (Int,Int)
type Trans = Pos -> Pos
type Pair a = (a,a)
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k==k']

-- 8.2 Data declarations
data Move = North | South | East | West
  deriving Show
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

-- *Main> moves [North, North, East, East, East, East] (3,4)
-- (7.6)

data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = Rect n n
-- *Main> :t Circle
-- Circle :: Float -> Shape
area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- Circle and Rect are //constructors//, which are functions w var input.
--constructor functions aren't defined in equations, which is what distinguishes them from other functions.

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (div m n)
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)
-- because in the standard prelude is
-- -- data Maybe a = Nothing | Just a

-- 8.3 Newtype declarations
{- "using newtype rather than data brings an efficiency benefit, because newtype constructors do not incur any cost when programs are evaluated- they are removed by the compiler once typechecking is completed." -} 
newtype Nat'' = N Int -- so N is the constructor,
-- *Main> :t N
-- Int -> Nat'

-- 8.4 recursive types
data Nat = Zero | Succ Nat
  deriving Show-- the usual peano/godel thing

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add'' :: Nat -> Nat -> Nat
add'' m n = int2nat (nat2int m + nat2int n)
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data List' a = Nil' | Cons' a (List' a)
len :: List' a -> Int
len Nil' = 0
len (Cons' _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Eq a => a -> Tree a -> Bool
occurs' x (Leaf y)     = x==y
occurs' x (Node l y r) = x==y || occurs' x l || occurs' x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- assuming search tree
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                 = x==y
occurs x (Node l y r) | x==y      = True
                      | x <y      = occurs x l
                      | otherwise = occurs x r

-- 8.5 class and instance declarations

{-
instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False  -}
-- only types declared w data and newtype can be made into instances of classes.
-- defaults can be overridden in instance declarations.

{- example of //class extension// in standard prelude
class Eq a => Ord a where
  (<),(<=),(>),(>=) :: a -> a -> Bool
  min, max          :: a -> a -> a
  min x y | x<=y      = x
          | otherwise = y 
  max x y | x<=y      = y
          | otherwise = x

instance Ord Bool where
  False < True = True
  _     < _    = False

  b<=c = (b<c) || (b==c)
  b> c = c< b
  b>=c = c<=b

-}

-- 8.6 basic Prop

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Equiv Prop Prop
  deriving Show

-- lets give us some trash data,
p1 :: Prop -- "A and notA"
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop -- "A and B implies A"
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop -- "A implies A and B"
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop -- "A and A implies B implies B" (modus ponens)
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 :: Prop -- "A or B iff B or A"
p5 = Equiv (Or (Var 'A') (Var 'B')) (Or (Var 'B') (Var 'A'))
-- these are not operations that can evaluate yet.
-- which is why we have to define //substitution// as a //lookup table//

type Subst = Assoc Char Bool -- recall Assoc from earlier section in this file.

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = (eval s p <= eval s q) && (eval s q <= eval s p)

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

-- 7.9.6
-- utils from ch7
type Bit = Int
unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (\x -> mod x 2) (\x -> div x 2)
rmDups :: Eq a => [a] -> [a]
rmDups []     = []
rmDups (x:xs) = x : (filter (/= x) (rmDups xs))

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin') range
  where
    range     = [0..(2^n)-1]
    make n bs = take n (bs ++ repeat 0)
    conv 0    = False
    conv 1    = True

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
--bools' 1 = [[False],[True]]
bools' n = (map (False :) (bools' (n-1))) ++ (map (True :) (bools' (n-1)))
--bools' n = False : bools' (n-1) ++ True : bools' (n-1) -- wrong
-- of course the book makes this look nicer w a where

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmDups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval x p | x <- substs p]
{-
*Main> isTaut p1
False
*Main> isTaut p2
True
*Main> isTaut p3
False
*Main> isTaut p4
True
-}


-- and it looks like exercise 8.9.8 is mission accomplished


-- 8.7 abstract machine
data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show
value :: Expr -> Int
value (Val n)    = n
value (Add x y)  = value x + value y
value (Mult x y) = value x * value y -- this evaluates left right. 

-- control stacks, for order of operations
type Cont = [Op]
data Op = EVAL1 Expr | ADD Int | EVAL2 Expr | MULT Int deriving Show
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

value' :: Expr -> Int
value' e = eval' e []

-- exercise 8.9.9 extend the abstract machine to support multiplication. 

-- 8.9.5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)               = f n
folde f g (Add e r) = g (folde f g e) (folde f g r)
folde f g (Mult e r) = g (folde f g e) (folde f g r)

-- 8.9.6

-- use folde define a function eval ::

eval'' :: Expr -> Int
eval'' = folde id (+)

size :: Expr -> Int
size = folde 1 (:) -- size isn't done yet.

{-
data Maybe = Just a | Nothing
-- -- alex demonstrating functor 
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f (Just a) = Just (f a)
fmap f Nothing = Nothing
-} 
--dummy data
ex :: Expr
ex = Add (Mult (Val 3) (Val 4)) (Val 7)
