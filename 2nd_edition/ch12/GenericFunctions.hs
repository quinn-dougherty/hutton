-- |

module GenericFunctions where
import Data.Char

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM f xs
                    return (y:ys)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = do b <- p x
                       ys <- filterM' p xs
                       return (if b then x:ys else ys)

-- powerset
powerset123 :: [[Int]]
powerset123 = filterM' (\x -> [True, False]) [1,2,3]

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

join' :: Monad m => m (m a) -> m a
join' mmx = do mx <- mmx
              x <- mx
              return x
