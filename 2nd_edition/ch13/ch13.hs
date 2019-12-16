-- exercise 1
module Ch13 where
import Parse
import Data.Char
import Control.Applicative

comment :: Parser ()
comment = do x <- item
             y <- item
             if x=='-' && y=='-' then
               do z' <- many (sat (\str -> isSpace str || isAlphaNum str))
                  z <- item
                  if z=='\n' then
                    return ()
                  else

                    return ()
             else
               return ()

testComment :: String
testComment = "-- comment \n"

