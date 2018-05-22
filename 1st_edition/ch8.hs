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

choice := apply first parser to input string, and if it fails apply the second one.

p +++ q is read "p or else q".

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                       [] -> parse q inp
                       [(v,out)] -> [(v,out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
          if p x then return x else failure

-- here, w sat, we can do things w "isDigit, isLower, isUpper, isAlphaNum" etc. 

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- grammar notes:
expr ::= expr + expr | expr * expr | (expr) | nat
nat ::= 0 | 1 | 2 | ...

-- this is arithmetic is ambiguous because of 2 * 3 + 4.

-- this solves the value ambiguity because it expresses order of operations, but it does not solve tree ambiguity because 1 + 2 + 3, while having the same value, has 2 different trees depending on assoc. 
expr ::= expr + expr | term
term ::= term * term | factor
factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...

-- This one is fully disambiguated:
nat ::= 0 | 1 | 2 | ...
factor ::= (expr) | nat
term ::= factor * term | factor
expr ::= term + expr | term

-- a simplicication: 
expr ::= term (+ expr | [] )
term ::= factor (* term | [] )
factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...
--where [] is epsilon or "" empty string.

-- if our inventory parsers were implemented, here it would be in literate haskell code: 
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ return t
term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            +++ return f
factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
             [(n,[])] -> n
             [(_,out)] -> error
             [] -> error "invalid input"

-- just notes, not gonna do exercises cuz i can't find the deprecated library. 

-} 

