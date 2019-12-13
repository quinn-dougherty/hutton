-- exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

{-
-- exercise 2-3, 6
 -- commented out because naming collision w standard library
instance Functor ((->) a) where
  -- fmap :: (a -> b) -> (a -> a) -> (a -> b)
  fmap = (.)

instance Applicative ((->) a) where
  -- pure :: b -> (a -> b)
  pure b = \_ -> b
  -- <*> :: (a -> b -> c) -> (a -> c) -> (b -> c)
  g <*> h = \x -> g x (h x)

instance Monad ((->) a) where
  -- (>>=) :: m b -> (b -> m c) -> m c
  -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  f >>= g = g . f

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

-- exercise 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap _ (Val i) = Val i
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> (Val i) = Val i
  (Var f) <*> (Var x) = Var (f x)
  (Var f) <*> (Add e1 e2) =

instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  e >>= me = ...

-- exercise 8 in MyState.hs
