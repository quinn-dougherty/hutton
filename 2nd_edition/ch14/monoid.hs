-- |
-- just copying what's in the standard Data.Monoid library
module Monoid where

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

{- laws:
  mempty `mappend` x = x
  x `mappend` mempty = x
  x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z
-}

instance Monoid [a] where
  -- mempty :: [a]
  mempty = []
  -- mappend :: [a] -> [a] -> [a]
  mappend = (++)

instance Monoid a => Monoid (Maybe a) where
  -- mempty :: Maybe a
  mempty = Nothing

  -- mappend :: Maybe a -> Maybe a -> Maybe a
  Nothing `mappend` my = my
  mx `mappend` Nothing = mx
  Just x `mappend` Just y = Just (x `mappend` y)

newtype Sum a = Sum a deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
  -- mempty :: Sum a
  mempty = Sum 0

  -- mappend :: Sum a -> Sum a -> Sum a
  Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product a deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

instance num a => Monoid (Product a) where
  -- memtpy :: Product a
  mempty = Product 1
  -- mappend :: Product a -> Product a -> Product a
  Product x `mappend` Product y = Product (x * y)
