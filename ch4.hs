
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

--even :: Integral a => a -> Bool
--even n = mod n 2 == 0

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a 
recip n = 1/n 

{-
abs :: Int -> Int
abs n = if n >= 0 then n else - n
-}

signum :: Int -> Int
signum n = if n < 0 then - 1 else 
  if n == 0 then 0 else 1

{- 
fst :: (a,b) -> a
fst (x,_) = x
-}

{-
(&&) :: Bool -> Bool -> Bool

implementation
b && b = b
_ && _ = False

IS INVALID because wildcard _ cannot be 2 different things... 

implementation

b && c | b == c = b
       | otherwise = False
IS VALID
-} 

test :: [Char] -> Bool
test ('a' : _) = True
test _ = False
-- a completely general function asking wether any [Char] begins w 'a'. length-independent. 

{-

null :: [a] → Bool 
null [] = True 
null (_:_) = False

head :: [a] → a
head (x:_)= x
tail :: [a] → [a]
tail (_:xs) = xs

-}

{-
pred :: Int -> Int 
pred 0 = 0
pred (n + 1) = n  
-}

oddsOne :: Int -> [Int]
oddsOne n = map f [0..n - 1] 
  where f x = x * 2 + 1
oddsTwo :: Int -> [Int]
oddsTwo n = map (\x -> x * 2 + 1) [0..n - 1]

-- Using library functions, define a function halve :: [ a ] → ([ a ], [ a ]) that splits an even-lengthed list into two halves.

halve :: [a] -> ([a],[a])
halve xs | mod (length xs) 2 == 0 = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)
         | otherwise = (xs,xs)

-- dummy data for testing halves
ys = [0,1,2,3,4,5,6,7]
xs = [1,2,3,4,5,6,7]

safetailCond :: [a] -> [a]
safetailCond xs = if null xs then xs else tail xs

safetailGuard :: [a] -> [a]
safetailGuard xs | null xs = []
                 | otherwise = tail xs

safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching xs = tail xs


-- disjunction pattern matching
d            :: Bool -> Bool -> Bool
d False False = False
d _ True    = True
d True _    = True
d b a       = a


{- problem: "" Multiple declarations of ‘d’ "", also when i try to use (d) instead of d i get "" Invalid type signature ...  Should be of form <variable> :: <type> "". 

-} 

{- translate 
True && True = True
_ && _ = False
into conditional rather than patternmatching. 

...

Then do 
True && b = b
False && _ = False
-}

c :: Bool -> Bool -> Bool
c a b = if a == True && b == True then True else False

c' :: Bool -> Bool -> Bool
c' a b = if a == True then b else 
           if a == False then False else False


mult3 :: Int -> Int -> Int -> Int
mult3 x y z = x * y * z

--given x
x = 4
xmult2 :: Int -> Int -> Int
xmult2 y z = x * y * z

--and then given y
y = 5
xymult1 :: Int -> Int
xymult1 z = x * y * z

-- as lambda
mult3Lambda :: Int -> Int -> Int -> Int
mult3Lambda x y z = (\p -> (\q -> (\r -> p * q * r) x) y) z