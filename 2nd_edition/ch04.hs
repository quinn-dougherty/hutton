--4.8.1
halve' :: [a] -> Maybe ([a],[a])
halve' xs | even (length xs) = Just (tops,bottoms)
          | otherwise = Nothing
 where
   tops    = take n xs
   bottoms = drop n xs
   n       = div (length xs) 2

--4.8.2
thirdA :: [a] -> a -- w head and tail
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a -- w list indexing
thirdB xs = xs !! 2

thirdC :: [a] -> a -- w pattern matching. 
thirdC (x:xs) = x -- damn i have no idea how to do this
-- ^^^ not a solution

safeTailA :: Eq a => [a] -> [a] -- conditional expression
safeTailA (x:xs) = if (x:xs) == [] then [] else xs
-- hrm again here we can't check empty list equality w/o asserting that a is an Eq a! there ought to be a way around this
-- Ok this currently gives ""*** Exception: ch4.hs:22:1-50: Non-exhaustive patterns in function safeTailA"" when you enter safeTailA []. hrm.

safeTailB :: Eq a => [a] -> [a] -- guard
safeTailB (x:xs) | (x:xs) == [] = []
                 | otherwise    = xs
-- same non-exhaustive pattern issue.

-- just for fun-- 
safeTailMonad :: (Eq a) => [a] -> Maybe [a]
safeTailMonad (x:xs) | (x:xs) == [] = Nothing
                     | otherwise    = Just xs
-- the problem w patterns here is certainly w "(x:xs) == []"

-- gonna have to actually read the chapter

--SafeTailC :: [a] -> [a] -- patternMatching.


-- 4.8.7
mult3 :: Int -> Int -> Int -> Int
mult3 x y z = (\p -> (\q -> (\r -> p*q*r) z) y) x

-- 4.8.8
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x*2 - 9
             | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (mod (sum [luhnDouble w, x, luhnDouble y, z]) 10) == 0
