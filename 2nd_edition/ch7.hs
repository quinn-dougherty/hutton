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

