-- |

module Ch14 where
import Data.Monoid
import Data.Foldable
import Data.Traversable
-- 1
instance (Monoid a, Monoid b) => Monoid (a, b) where
  --mempty :: (a, b)
  mempty = (mempty, mempty)

  -- mappend :: (a, b) -> (a, b) -> (a, b)
  (x,y) `mappend` (x', y') = (x `mappend` x', y `mappend` y')

-- 2
instance Monoid b => Monoid a -> b where
  -- mempty :: a -> b
  mempty = \x -> mempty

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = \x -> f x `mappend` g x

-- 3
instance Foldable Maybe where
  --fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty
  fold (Just x) = x

  --foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap f Nothing = mempty
  foldMap f (Just x) = f x

  --foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr g v Nothing = v
  foldr g v (Just x) = g x v

  --foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl g v Nothing = v
  foldl g v (Just x) = g v x

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> Maybe b
  traverse _ Nothing = Nothing
  traverse g (Just x) = pure Just <*> g x


-- 4
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  -- foldMap :: Monoid b -> (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

-- 5 - using foldMap define a generic version of `filter` on lists to be used w any foldable type
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if f x then x : (filter f xs) else x : (filter f xs)

-- foldMap :: Monoid b => (a -> b) -> t a -> b
-- this is currently wrong
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f xs = foldMap f xs
