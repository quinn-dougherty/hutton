-- |
-- just coppying from standard library again
module Foldables where

class Foldable t where
  fold :: Monoid a => t a -> a
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (a -> b -> a) -> a -> t b -> a

instance Foldable [] where
  -- fold :: Monoid a => [a] -> a
  fold [] = mempty
  fold (x:xs) = x `mappend` fold xs

  -- foldMap :: Monoid b => (a -> b) -> [a] -> b
  foldMap _ [] = mempty
  foldMap f (x:xs) = f x `mappend` foldMap f xs

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ v [] = v
  foldr g v (x:xs) = g x (foldr g v xs)

  -- foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl _ v [] = v
  foldl g v (x:xs) = foldl g (g v x) xs

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = (fold l) `mappend` (fold r)

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = (foldMap f l) `mappend` (foldMap f r)

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr g v (Leaf x) = g v x
  foldr g v (Node l r) = foldr g (foldr g v r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl g v (Leaf x) = g v x
  foldl g v (Node l r) = foldl g (foldl g v l) r
