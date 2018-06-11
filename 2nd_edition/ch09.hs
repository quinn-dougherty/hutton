data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y ==0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
-- the point of [Int] as return type is strictly so that
-- success denoted by singleton list
-- failure denoted by empty list
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

--dummy data
ex :: Expr
ex = App Add (Val 1) (App Mul (Val 2) (Val 3))

ex' :: Expr
-- the commutatiity consideration forces x<=y for x*y, so we have to switch out 51 and 15
ex' = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 1) (Val 50))
-- ca't figure out why or how eval ex' returns []

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss -- 2^n in input length. x
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]] -- n+1 in input length 
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]] -- factorial in input length
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs -- This growth is:
{- length xs  =>   choices xs
     1                  2
     2                  5
     3                  16
     4                  65
     5                 326
-- i don't understand this function
-}

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

{-
*Main> solution ex' [1,3,7,10,25,50] 765
True
-}

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                 l      <- exprs ls,
                 r      <- exprs rs,
                 e      <- combine l r]
 
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int) -- now to optimize

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns, -- make sure this is split and not split' 
                    lx      <- results ls,
                    ry      <- results rs,
                    res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]
{-
main :: IO () -- i can't abstract this out into another file like it says in the book
main = print (solutions'' [1,3,7,10,25,50] 765)
-- solutions' is supposed to be optimized pretty well
-} 
-- further optimzation by going up to valid and altering it for commutativity 

--- EXERCISES ---
-- 9.11.1 -- redefine the combintatorial fnction choices using a list comprehension rather than using composition, concat, map.

choices' :: [a] -> [[a]]
choices' xs = concat [perms ys | ys <- (subs xs)]
-- hrm it doesn't want me to use concat tho.

-- 9.11.2 -- isChoice :: Eq a => [a] -> [a] -> Bool
-- which decides if one list is chosen from another
--- w/o using perms and subs.

-- remove first occurrence from list.
rmElem :: Eq a => a -> [a] -> [a]
rmElem x' [] = []
rmElem x' (x:xs) | x==x'     = xs
                 | otherwise = x : rmElem x' xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice xs ys | elem xs (choices ys) = True
               | otherwise            = False
-- now w/o using choices

{-
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss -- 2^n in input length. x
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]] -- n+1 in input length 
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]] -- factorial in input length
perms (x:xs) = concat (map (interleave x) (perms xs))
-}   
isChoice' :: Eq a => [a] -> [a] -> Bool
isChoice' [] _      = True
isChoice' _ []      = False
isChoice' xs ys = and [elem x ys | x <- xs]
-- looks like that's right. 

-- 9.11.3
-- if split also returned pairs containing the empty list, solutions would
split' :: [a] -> [([a],[a])]
split' xs = [(take n xs, drop n xs) | n <- [0..(length xs)]]
-- when split' is used, the thing runs forever / crashes!
-- probably because one of the helper functions didn't control for []

--9.11.4
--for
nmbs :: [Int]
nmbs = [1,3,7,10,25,50]
--verify that
numPossExprs :: Int
numPossExprs = 33665406
numVldExprs :: Int
numVldExprs = 4672540
--using choices, exprs, eval
{-
*Main> (length . choices) [1..6]
1957
*Main> (length . exprs) [1..6]
43008
-}

{-
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]
-}

test1 :: Bool
test1 = numVldExprs == (length . choices) [1..2] -- hrm why is this false
test2 :: Bool
test2 = False

-- use choices . eval
