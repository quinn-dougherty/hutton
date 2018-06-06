
-- 7.9.5
{-
curry2 :: ((a,b)->c) -> (a->b->c)-- I would have thought this was the type signature, but :t curry returns the RHS w no parenthesis. 
curry2 = \x -> 

uncurry2 :: (a->b->c) -> ((a,b) -> c)
uncurry2 f = 
-}
-- from wiki.haskell.org:
  {- 

    Simplify curry id
     
    Simplify uncurry const
    Express snd
    using curry
    or uncurry
    and other basic Prelude functions and without lambdas
    Write the function \(x,y) -> (y,x)
    without lambda and with only Prelude functions 
-}

id' :: a->a
id' x = x
--curryId :: 


{- -- const is the set of constant functions
uncurryConst :: a -> c
uncurryConst b = 
-}
div11 :: Int -> Int
div11 k = div k 11

--snd' :: (a,b) -> b
--snd' p = curry (id p)

curry' :: ((a,b)->c) -> a -> b -> c
curry' op x y = op (x,y)

uncurry' :: (a->b->c) -> (a,b) -> c
uncurry' op t = op (fst t) (snd t)
