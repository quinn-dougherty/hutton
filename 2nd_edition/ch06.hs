-- 6.8.4 Euclid
euclid :: Int -> Int -> Int
euclid p q | p == q           = q
           | p <  q && p >= 0 = euclid p (q-p)
           | p >  q && q >= 0 = euclid q (p-q)
           | otherwise        = min p q-- hrm.. this shouldn't be an infinite recursion

