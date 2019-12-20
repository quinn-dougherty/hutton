-- exercise 1
module Ch13 where
import Parse
import Data.Char
import Control.Applicative

comment :: Parser ()
comment = do sat (== '-')
             sat (== '-')
             many (sat (\c -> isSpace c || isAlphaNum c))
             return ()

testComment :: String
testComment = "-- this is a comment \n"

-- exercise 5
data Expr = Val Int | Add Expr Expr | Mult Expr Expr | Subt Expr Expr

-- exercise 6 in Parse.hs
