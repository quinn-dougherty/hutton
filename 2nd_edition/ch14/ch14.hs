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
instance Monoid b => Monoid (a -> b) where
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

