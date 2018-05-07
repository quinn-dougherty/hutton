import Data.Char
--import Data.Int

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

 
concatMy :: [[a]] -> [a]
concatMy xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

{- 
length    :: [a] -> Int
length xs = sum [1 | _ <- xs]
-}

factors  :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

prime   :: Int -> Bool
prime n = factors n == [1,n]

primes  :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find     :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- find 'b' [ (’a’, 1), (’b’, 2), (’c’, 3), (’b’, 4) ] 
-- should return [2,4]

-- pg 54. 

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
  where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int] -- with find -- exercise 5.7.6
positions' x xs = [i | i <- find x (zip xs [0..(length xs - 1)])]

-- Strings

lowers :: String -> Integer
lowers xs = toInteger (length [x | x <- xs, isLower x])


count :: Char -> String -> Integer
count x xs = toInteger (length [x' | x' <- xs, x == x'])

-- 5.5 the ceasar cipher!!

let2int :: Char -> Int
let2int c = ord c - ord 'A'
int2let :: Int -> Char
int2let n = chr (ord 'A' + n) 

shift :: Int -> Char -> Char
shift n c | isLetter c = int2let (mod (let2int c + n ) 52)
          -- | isUpper c = int2let (mod (let2int c + n) 52)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs] 

a :: String
a = "typE camEra PLaton B0B trnN"

b :: String
b = encode 5 a
-- encode (-5) b returns a

decode :: Int -> String -> String
decode n xs = [shift (-n) x | x <- xs]


-- Frequency tables
table :: [Float]
table =  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Integer -> Integer -> Float
percent n m = (fromInteger n / fromInteger m) * 100
-- fromInt :: Int -> Float casts int to float.

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table''' :: [Float]
table''' = freqs b

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..51]]
    table' = freqs xs
-- holy shit that's sick

--exercises

--5.7.1
sigmaFrom0toNofKSquared :: Int -> Int
sigmaFrom0toNofKSquared n = sum [x ^ 2 | x <- [0..n]]

-- 5.7.2
replicate' :: (Num k, Enum k) => k -> a -> [a]
replicate' n x = [x | _ <- [0..n]]

-- 5.7.3
pyths :: Int -> [(Int, Int, Int)]
pyths p = [(x,y,z) | x <- [1..p], y <- [1..p], z <- [1..p], (x ^ 2) + (y ^ 2) == (z ^ 2)]

--5.7.4
perfects :: Int -> [Int]
perfects n = [p | p <- [0..n], sum (factors p) - p == p]

-- 5.7.5
w :: [(Int,Int)]
w = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- is a single comprehension with two generators (one "such that" statement and two"is drawn from" statements)


-- is 2 comprehensions with a single generator.

w'' :: [(Int,Int)]
w'' = zip [x | x <- [1,2,3]] [y | y <- [4,5,6]]
-- is some third thing.        
--5.7.6 : see line 46. 

-- 5.7.7
timesBin :: (Int,Int) -> Int
timesBin (a,b) = a*b

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [timesBin (x,y) | (x,y) <- zip xs ys]

-- 5.7.8 -- modify the ceasear cipher program to also handle upper case letters.
-- so what i did was i went into let2int and int2let and changed it from 'a' to 'A' because "ord 'A' < ord 'a' ". I also had to change the modulus from 26 to 52 in shift. 
