import Data.Char

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> (Int -> Int)
add'' = \x -> (\y -> x + y)

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- map (map (+1)) [[1,2,3], [4,5]] >>> [[2,3,4], [5,6]]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs

-- Type pxs = (a -> Bool) -> [a] -> [a]

filter' :: Integral a => (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x] -- doesn't seem to be working.

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                 = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map'' (^2) (filter'' even ns))

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

--
foldr'            :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)
-- sum, product, or, and.

--length revisited.
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0

-- reverse revisited
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse'        :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = snoc x (reverse' xs)

reverse''  :: [a] -> [a]
reverse'' = foldr snoc [] 

--"(++ys) = foldr (:) ys"
-- (xs++) = foldl (\ys y-> ys++[y]) xs

-- foldl is for left-associating binary operators.
sum' = sgma 0
  where
    sgma v [] = v
    sgma v (x:xs) = sgma (v+x) xs

-- pattern:
-- f v [] = v
-- f v (x:xs) = f (v @ x) xs
-- -- where v is the accumulator value and @ is some operator.

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0
-- product, or, and, etc.

length''l :: [a] -> Int
length''l = foldl (\n _ -> n+1) 0
reverse''l :: [a] -> [a]
reverse''l = foldl (\xs x -> x : xs) []
-- (xs++) :: [a] -> [a]
-- (xs++) = foldl (\ys y-> ys++[y]) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- COMPOSITION
pipe :: (b->c) -> (a->b) -> (a->c)
pipe f g = \x -> f (g x)

pipe3 :: (c->d) -> (b->c) -> (a->b) -> a->d
pipe3 f g h = \x -> f (g (h x))
--  (pipe3 sum (map (^2)) (filter even)) == sumsqreven'

composeUniformlyTypedFunctions :: [a->a] -> (a->a)
composeUniformlyTypedFunctions = foldr (.) id

-- 7.6 Base Conversion
type Bit = Int

make8      :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
chop8      :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

bin2IntBckwrds :: [Bit] -> Int
bin2IntBckwrds bits = sum [w * b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

-- iterate f x = [x, f x, f.f x, f.f.f x, ... ]
bin2int'Bckwrds :: [Bit] -> Int
bin2int'Bckwrds = foldr (\x y -> x + 2*y) 0
{-
bin2Int :: [Bit] -> Int
bin2Int =
-}

int2BinBckwrds :: Int -> [Bit]
int2BinBckwrds 0 = []
int2BinBckwrds n = mod n 2 : int2BinBckwrds (div n 2)

encode :: String -> [Bit]
encode = concat . map (make8 . int2BinBckwrds . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int'Bckwrds) . chop8

transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel = id

--7.8.1
-- express [f x | x <- xs, p x], for some f and p, in map & filter

-- mf f p xs = [f x | x <- xs, p x]
-- -- where
-- -- f :: a -> b
-- -- p :: a -> Bool
-- -- xs :: [a]

mf :: (a->b) -> (a->Bool) -> [a] -> [b]
mf f p xs = [f x | x <- xs, p x]
mf' :: (a->b) -> (a->Bool) -> [a] -> [b]
mf' f p xs = map f . filter p [xs]
--7.8.2 -- done lines 34-58 of this file.

-- 7.8.3
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = [f x | x <- xs]

