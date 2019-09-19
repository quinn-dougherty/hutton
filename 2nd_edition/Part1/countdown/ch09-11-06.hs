-- exercise # 6
---- a. allow exponentiation in expressions
---- b. produce nearest solutions if no exact solution is possible
---- c. order solutions from most simple to least simple, for some simplicity measure.

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x /= 0 && y /= 0 && x <= y -- cuz we don't want to double-count on account of commutativity, and neutrality is considered redundant
valid Sub x y = x > y -- no negative numbers
valid Mul x y = x /= 1 && y /= 1 && x <= y -- neutrality is considered redundant, wasteful.
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 0 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

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
-- the point of [Int] as return type is strictly so that success can be denoted by singleton list and failure denoted by empty list.
eval (Val n)  = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- copying combinatorial functions from before
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
choices = concat . map perms . subs 

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

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
ops = [Add,Sub,Mul,Div,Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = Expr -- now to optimize

results :: [Int] -> [Result]
results []  = []
results [n] = [Val n | n > 0]
results ns = [res | (ls,rs) <- split ns, -- make sure this is split and not split' 
                    lx      <- results ls,
                    ry      <- results rs,
                    res     <- combine lx ry]


--dummy data
ex :: Expr
ex = App Add (Val 1) (App Mul (Val 2) (Val 3))

ex' :: Expr
-- the commutatiity consideration forces x<=y for x*y, so we have to switch out 51 and 15
ex' = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 1) (Val 50))



-- definitely finished part a.

-- still to do part b and c
