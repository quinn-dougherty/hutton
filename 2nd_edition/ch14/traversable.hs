-- |

module Traversable where

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

instance Traversable [] where
  -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse g [] = pure []
  traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> Tree b
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r
