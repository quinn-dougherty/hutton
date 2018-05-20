factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- defining * recursively.
mult :: Int -> Int -> Int
mult m 0 = 0
mult m n = m + (mult m (n - 1)) 

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip'               :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop (n - 1) xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b >  x]

even'   :: Int -> Bool
even' 0 = True
even' n = odd (n-1)
odd'    :: Int -> Bool
odd'  0 = False
odd'  n = even (n-1)

-- positions, not vals. duh, because it didn't say Ord a => in the beggining. 
evens'        :: [a] -> [a]
evens'     [] = []
evens' (x:xs) = x : odds' xs
odds'         :: [a] -> [a]
odds' []      = []
odds' (_:xs)  = evens' xs

{-
product'        :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product ns
-}
product'' :: Num a => [a] -> a
product'' = foldr (*) 1

drop'' :: Integral b => b -> [a] -> [a]
drop'' 0 xs     = []
drop'' n []     = []
drop'' n (_:xs) = drop'' (n - 1) xs

init' :: [a] -> [a]
init' (x:xs) | null xs   = []
            | otherwise = x : init xs

init'' :: [a] -> [a]
init'' [_] = []
init'' (x:xs) = x : init xs

-- exercises

-- 6.8.1
exp' :: (Integral b, Num a) => a -> b -> a
exp' _ 0 = 1
exp' x n = x * (exp' x (n - 1))

-- 6.8.2
--on paper

-- 6.8.3
and' :: [Bool] -> Bool
and' [] = True
and' (True:bs) = and' bs
and' (False:bs) = False
{-
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [x:xs] = x : concat' xs
-}
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 p = [p]
replicate' n p = p : replicate' (n-1) p

enth' :: [a] -> Int -> a -- "(!!)" 
enth' (x:xs) 0 = x
enth' (x:xs) n = enth' xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | x == a    = True
               | otherwise = elem' a xs
-- 6.8.4-5
axs :: [Int] -- dummy data
axs = [1,65,3,7,45,2,6,65,8,3,4,5,0,9,1,7,3,6,5,8,0,4,3]

merge :: Ord a => ([a],[a]) -> [a]
merge ([], ys)          = ys
merge (xs, [])          = xs
merge (xs, ys) | head xs <= head ys = (head xs) : merge (tail xs, ys)
               | head ys <  head xs = (head ys) : merge (xs, tail ys)

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take k xs, drop k xs)
  where
    k = div (length xs) 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ts, msort ds)
  where
    ts = ((fst . halve) xs)
    ds = ((snd . halve) xs) -- not done

-- 6.8.6
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' n (x:xs) = x : (take' (n-1) xs)

last' :: [a] -> a
last' xs = enth' xs n where n = (length xs) - 1
