--3.11.1
{-
[(False,'0'), (True,'1')] :: [(Bool,Char)]
([False, True], ['0', '1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a]->[a]]
-}
--3.11.2
bols :: [Bool]
bols = [True, False, True, False]

nums :: [[Int]]
nums = [ (filter even [1..n]) | n <- [1..30]]

add' :: Int -> Int -> Int -> Int
add' x y z = x+y+z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a->b) -> a -> b
apply f x = f x

--3.11.3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: String -> Bool
palindrome xs = reverse xs == xs

twice :: (a->a) -> a -> a
twice f x = f (f x)

--3.11.5
{- equality of two total functions means if y=fx and y=fx' then x=x', or if y=fx and y'=fx tehn y=y'. this isn't "feasible" usually because of infinity, but some total functions have a finite list of input/output pairs-- for these, equality of functions would be fine. 
