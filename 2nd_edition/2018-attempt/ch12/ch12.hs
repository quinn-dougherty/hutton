-- monads, functors, applicaive -- ch12 exercises

-- 12.5.1

-- take the type binary tree w data in nodes:
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  --fmap :: (a->b) -> f a -> f b
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
--looks about right - i wonder if it compiles.

--12.5.2 declare instance to make partially applied function type (a ->) into a functor:
{-
instance Functor (arrow a) where
  --fmap (a->b) -> (arrow a)  a -> (arrow a) b
  fmap pf x = pf fmap x -- even if this compiles i won't feel like its right 
  
-} -- makes no sense

--12.5.3
{-instance Applicative arr where
  pure
  <*>
-- pure is K combinator, <*> is S combinator in combinatory logic

    -}


--12.5.4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a->b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  --pure :: a -> ZipList a
  pure x = Z [x]
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (<*>) (Z gs) (Z xs) = Z [g x | g <- gs, x <- xs]
-- currently gets error " '<$>' is not a visible method of class Applicative " 

