--1.7.2
{- for all x :: Num
  sum [x]
= x + sum []
= x + 0
= x
-}

--1.7.3 trivial
product' :: Num a => [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

--1.7.4 reverse qsort
dummy1 :: Num a => [a]
dummy1 =  [4,2,6,5,7,1,9,34,8,12,56,34,5,2,1,1,2,34,56,8]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [p | p <- xs, p <= x]
    larger  = [q | q <- xs, q >  x]

reverseQSort :: Ord a => [a] -> [a]
reverseQSort [] = []
reverseQSort (x:xs) = reverseQSort larger ++ [x] ++ reverseQSort smaller
  where
    larger  = [p | p <- xs, p >  x]
    smaller = [q | q <- xs, q <= x]


--1.7.5
-- replacing the "or equal to" aspect of qsort defs eliminates repeats. qsort dummy1 in this situation returns [1,2,4,5,6,7,8,9,12,34,56]


