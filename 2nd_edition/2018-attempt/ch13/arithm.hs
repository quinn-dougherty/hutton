-- 13.8 Arithmetic Expressions
module Arith where
import Ch13

--k = parse nats " [1,2,3,5,6] fkjdlajf "
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
             <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- exr
            symbol ")"
            return e
          <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n,[])] -> n
            [(_,out)] -> error ("Unused input " ++ out)
            [] -> error "invalid input"

