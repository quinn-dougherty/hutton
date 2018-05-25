import Data.Char
import Data.List

-- this i need to learn better cuz i never finished the exercises in 1st edition.

curriedAdd :: Int -> Int -> Int
curriedAdd x y = x + y

uncurriedAdd :: Int -> (Int -> Int)
uncurriedAdd = \x -> (\y -> x + y)

twice :: (a->a) -> a -> a
twice f x = f (f x) {-
*Main> :type twice (*2)
twice (*2) :: Num a => a -> a 

*Main> :type map (+1)
map (+1) :: Num b => [b] -> [b]  -}

-- foldl, foldr as alternative to recursion
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs
-- and its ilk can be expressed
and'' :: [Bool] -> Bool
and'' = foldr (&&) True --

{- in general,

"G :: [a] -> a
G = foldr op e
"
where op :: (a,a) -> a and for A :: a, A op e == A, e op A == A
-}

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs){-
*Main> :type foldr (+) 0
foldr (+) 0 :: (Foldable t, Num b) => t b -> b

*Main> :type foldr' (+) 0
foldr' (+) 0 :: Num b => [b] -> b

where H := foldr' (+) 0,
  H (1:(2:(3:[])))
=   (1+(2+(3+0 )))

in other words, it converts cons to op and [] to the neutral e.
except in the case of length where it converts each element,whatever the type, into a 1::Int. 

-}

altSum :: Num a => [a] -> a
altSum = sum' 0
  where
    sum' v []     = v
    sum' v (x:xs) = sum' (v+x) xs

{-
f v [] = v
f v (x:xs) = f (v # x) xs
  where # is some op and v is called the accumulator value.
-}

-- for functions like sum, and, etal that can be understood either as foldl or foldr, the choice is made strictly for efficiency reasons.


odd' :: Integral a => a -> Bool
odd' = not . even


-- simulating transmission of chars as bin digits.
type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- (make8 . int2bin) 13 == [1,0,1,1,0,0,0,0]

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- First past the post vote counting
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmDups :: Eq a => [a] -> [a]
rmDups []     = []
rmDups (x:xs) = x : (filter (/= x) (rmDups xs))

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmDups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- ALTERNATIVE VOTE
ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmEmpty :: Eq a => [[a]] -> [[a]]
rmEmpty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmEmpty bs) of
               [c]    -> c
               (c:cs) -> winner' (elim c bs)
-- copying some over from when i did these in 1st edition

--7.8.1
-- express [f x | x <- xs, p x], for some f and p, in map & filter

-- mf f p xs = [f x | x <- xs, p x]
-- -- where
-- -- f :: a -> b
-- -- p :: a -> Bool
-- -- xs :: [a]

mf :: (a->b) -> (a->Bool) -> [a] -> [b]
mf f p xs = [f x | x <- xs, p x]
--mf' :: (a->b) -> (a->Bool) -> [a] -> [b]
--mf' f p xs = map f . filter p xs

mf' :: (a->b) -> (a->Bool) -> [a] -> [b]
mf' f p xs = map f (filter p xs)

{- 
fm :: (b->Bool) -> (a->b) -> [a] -> [b]
fm p f xs = [p y | y <- ys]
  where
   ys = [f x | x <- xs] -- I can't figure out why this isn't working
-}

fm' :: (b->Bool) -> (a->b) -> [a] -> [b]
fm' p f xs = filter p (map f xs)

--7.8.2
all' :: (a -> Bool) -> [a] -> Bool
all' p []           = True
all' p (x:xs) | p x = and (True : all' p xs : [])
              | otherwise = False

any' :: (a -> Bool) -> [a]-> Bool
any' p [] = False
any' p (x:xs) | p x       = True
              | otherwise = or (False : any' p xs : [])

dummy1 = [2,4,6,7,8,9]
dummy2 = [1..10]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = [] -- fixed it.
                    
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs -- 


-- 7.8.3 -- 
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = [f x | x <- xs]
mapf :: Num a => [a] -> [a]
mapf xs = [f x | x <- xs]
  where f = (*2) -- hrm... not too satisfying.

mapf' :: (a->b) -> ([a]->[b])
mapf' = \f -> map f -- i think this is what they meant. 

--7.8.4 -- way not done. 
xOPy :: Num a => (a->a->a) -> (a,a) -> a
xOPy op (x,y) = op x y
dec2Int' :: [Int] -> Int
dec2Int' xs = sum [xOPy (*) w | w <- zip n xs]
  where
    n = [10 ^ (p-k) | k <- [0..]]
    p = (-1) + length xs

-- map (xOPy (*)) (zip [1,2,3,4,5] [1, 10, 100, 1000, 10000]) == [1,20,300,4000,50000]

{-
dec2Int :: [Int] -> Int
dec2Int =  foldl (+) (map (xOPy (*)) (zip _ [10 ^ (p-k) | k <- [0..]]))
--  where
--    n = [10 ^ (p-k) | k <- [0..]]
--    p = (-1) + length xs
-}

-- 7.8.5
{-
curry2 :: ((a,b)->c) -> (a->b->c)-- I would have thought this was the type signature, but :t curry returns the RHS w no parenthesis. 
curry2 = \x -> 

uncurry2 :: (a->b->c) -> ((a,b) -> c)
uncurry2 f = 
-}
-- from wiki.haskell.org:
  {- 

    Simplify curry id
     
    Simplify uncurry const
    Express snd
    using curry
    or uncurry
    and other basic Prelude functions and without lambdas
    Write the function \(x,y) -> (y,x)
    without lambda and with only Prelude functions 
-}

id' :: a->a
id' x = x
--curryId :: 


{- -- const is the set of constant functions
uncurryConst :: a -> c
uncurryConst b = 
-}
div11 :: Int -> Int
div11 k = div k 11

--snd' :: (a,b) -> b
--snd' p = curry (id p)


-- 7.8.6
unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
-- redefine chop8, mapf, iteratef using unfold. 

--7.9.9 -- altMap
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g [] = []
altMap f g [_] = [] -- this is necessary to prevent calling head on empty list. 
altMap f g xs =
    (f . head) xs
  : (g . head . tail) xs
  : altMap f g (twice tail xs) 

-- 7.9.10 -- altMap and luhn.              (Luhn from ch4.hs)  -- 4.8.8
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x*2 - 9
             | otherwise = x*2

luhn4 :: Int -> Int -> Int -> Int -> Bool
luhn4 w x y z = (mod (sum [luhnDouble w, x, luhnDouble y, z]) 10) == 0

luhn :: [Int] -> Bool -- of any length
luhn = \xs -> (mod (sum (altMap luhnDouble id xs)) 10) == 0
-- haven't tested this yet.
