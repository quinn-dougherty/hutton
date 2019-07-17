module Ch13 where
import Control.Applicative
import Data.Char

-- by convention, singleton list denotes success and empty list denotes failure

--type Parser a = String [(a,String)] 

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x,xs)])

--13.4: Sequencing
instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure : a -> Parser a
  pure v = P (\inp -> [(v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g,out)]-> parse (fmap g px) out)

three :: Parser (Char, Char)
-- a parser that consumes three characters,discards the second, and returns the first and third as a pair
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (f v) out)
-- `p >>= f` is a parser that fails if the aplication of the parser p to the input string inp fails, and otehrwise applies the function f to the result value v to give another parser f v, which is then applied to the output string `out` that was produced by the first parser to give teh final result.
{-
instance Alternative Maybe where
  --empty :: Maybe a
  empty = Nothing
  -- <|> :: Maybe a -> Maybe a -> Maybe a
  Nothing <|> my = my
  (Just x) <|> _ = Just x

-} 

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- <|> : :Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

-- 13.6 - Derived Primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x
-}
ident :: Parser String
-- parser for variable names in the family ll*#* where `ll*` represents 1 or more letters and `#*` represents 0 or more numbers 
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

--13. handling spacing
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)
