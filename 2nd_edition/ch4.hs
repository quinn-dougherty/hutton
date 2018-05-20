--4.8.1
halve' :: [a] -> ([a],[a])
halve' xs = (tops,bottoms)
  where
    tops <- take n xs
    bottoms <- drop n xs
    n <- div (length xs) 2
