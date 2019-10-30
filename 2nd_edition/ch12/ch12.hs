-- exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)


{-
-- exercise 2-3

instance Functor ((->) a) where
  -- fmap :: (a -> b) -> (a -> a) -> (a -> b)
  fmap = (.)

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure b = \_ -> b
  -- <*> :: (a -> b -> c) -> (a -> c) -> (b -> c)
  g <*> h = \x -> g x (h x)
-}

-- exercise 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x | _ <- [1..]]
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

{- exercise 5: work out types of each term in applicative laws.
-}
