-- |

module Ex8 where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v, out)] -> [(g v, out)]
                   )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g, out)] -> parse (fmap g px) out
                    )

instance Monad Parser where
  -- >>= Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v, out)] -> parse (f v) out
                  )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v, out)] -> [(v, out)]
                  )

item :: Parser Char
item = P (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x, xs)]
         )

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit


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

symbol :: String -> Parser String
symbol xs = token (string xs)

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

integer :: Parser Int
integer = token int

differand :: Parser Int
differand = do symbol "("
               e <- expr
               symbol ")"
               return e
            <|> integer

expr :: Parser Int
expr = do e1 <- differand
          (do symbol "-"
              e2 <- expr
              return (e1 - e2))
           <|> return e1

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"

-- the task- rewrite w `many` and `foldl`
