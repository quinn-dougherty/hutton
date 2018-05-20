{- NOTES
type Parser'   = String -> Tree
type Parser''  = String -> (Tree,String)
type Parser''' = String -> [(Tree,String)]
-- where Tree is arbitrary ast's.
-- -- Parser' has to consume its entire argument, whereas Parser'' and Parser''' can consume part of it and return the rest of it.
type Parser a = String -> [(a,String)]


return' :: a -> Parser a
return' v = \inp -> [(v,inp)] -- Always scceeds. 

failure' :: Parser a
failure' = \inp -> [] -- always fails. 

item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x,xs)]

parse' :: Parser a -> String -> [(a,String)]
parse' p inp = p inp

-- ***-------- need to import their library, sometime when i have wifi.
{-Prelude> :i >>=
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  ...
  	-- Defined in ‘GHC.Base’
infixl 1 >>=                      -}

(>>=') :: Parser a -> (a -> Parser b) -> Parser b
p >>=' f = \inp -> case parse p inp of
                        [] -> []
                        [(v,out)] -> parse (f v) out

-- consume 3 chars, discard 2nd, return 1st & 3rd as pair
p :: Parser (Char,Char)
p = do x <- item
       item
       y <- item
       return (x,y)




-} 
