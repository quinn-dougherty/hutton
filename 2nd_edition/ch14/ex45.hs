-- |

module Ex45 where
import Data.Foldable

-- 4
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)


instance Foldable Tree where
  -- foldMap :: Monoid b -> (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

-- 5 - using foldMap define a generic version of `filter` on lists to be used w any foldable type
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : (filter' f xs) else (filter' f xs)

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f xs = filter' f (toList xs)
-- but it asks me to use foldmap

-- foldMap :: Monoid b => (a -> b) -> t a -> b
--filterF' :: Foldable t => (a -> Bool) -> t a -> [a]
--filterF' p xs =


-- for testing
tree :: Tree Int
tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 (Node Leaf 2 Leaf))

main5 :: IO ()
main5 = do let inp = tree
           let outp = filterF (\x -> x `mod` 2 == 0) inp
           putStrLn (show inp)
           putStrLn " but just evens "
           putStrLn (show outp)

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

main4 :: IO ()
main4 = do let inp = tree
           let outp = traverse dec tree
           putStrLn (show inp)
           putStrLn " decremented  "
           putStrLn (show outp)
