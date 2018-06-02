last' :: Eq a => [a] -> a
last' (x:xs) | xs == []  = x
             | otherwise = last' xs
-- shouldn't have to need Eq a, but testing for empty list requires it. that's annoying.

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x : (init' xs)
