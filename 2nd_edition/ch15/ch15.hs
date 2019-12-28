-- |

module Ch15 where

-- 4
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))


-- 5
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node (repeatTree x) x (repeatTree x)

takeTree :: Int -> Tree a -> Tree a
takeTree _ Leaf = Leaf
takeTree 0 _ = Leaf
takeTree n (Node Leaf x r) = Node Leaf x (takeTree (n-1) r)
takeTree n (Node l x Leaf) = Node (takeTree (n-1) l) x Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-2) r)


replicateTree :: Int -> a -> Tree a
replicateTree 0 x = Leaf
replicateTree 1 x = Node Leaf x Leaf
replicateTree n x = Node (replicateTree n' x) x Leaf where n'=n-1

-- for testing
tree :: Tree Int
tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 (Node Leaf 2 Leaf))

testReplicate :: IO ()
testReplicate = do
  let inp = replicateTree 20 1
  let taken = takeTree 5 inp
  putStrLn (show taken)


-- 6 newton's method
guess :: Double -> Double -> Double
guess n curr = next curr
  where
    next = \x -> (x + n/x) / 2

sqroot :: Double -> Double
sqroot n = approxList !! k
  where
    approxList = iterate (guess n) (n/2)
    k = 5

testNewton :: IO ()
testNewton = do
  let inp = 16
  let approxList = take 5 (iterate (guess inp) 8)
  putStr ("The approximate square root of " ++ (show inp) ++ " is ")
  putStrLn (show approxList)
