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

