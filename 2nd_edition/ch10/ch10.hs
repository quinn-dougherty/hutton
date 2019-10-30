import Data.Char
import System.IO

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters. "

-- ex 10.10.1
{-
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   putStr xs
-}
-- use sequence_ :: [IO a] -> IO a
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 10.10.4
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     putChar '\n'
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "ERROR: Invalid digit"
                          getDigit prompt

accu :: Int -> Int -> IO Int
accu x n = if n<=0 then
             -- base case
             return x
           else
             -- recursive step
             do
               -- get the number to add
               x'' <- getChar
               -- clear out the screen
               putChar '\n'
               -- convert x'' to int
               let x' = digitToInt x''
               -- accumulate and recursively continue.
               accu (x+x') (n-1)

adder :: IO ()
-- assuming input is always valid.
adder = do putStr "How many numbers? "
           -- get the number of steps to take
           n' <- getChar
           let n = digitToInt n'
           putChar '\n'

           -- run the accumulation
           a' <- accu 0 n
           let a = show a'

           -- report
           putStr "The total is "
           putStrLn a

           return ()

-- 10.10.5 -- redefine adder with the function sequence
adder' :: IO ()
adder' = do putStr "How many numbers? "
            -- get the number of steps to take
            n' <- getChar
            let n = digitToInt n'
            putChar '\n'

            let a' = [do x' <- getChar
                         putChar '\n'
                         return x'
                     | _ <- [1..n]
                     ]

            -- convert the list of io actions into
            -- an io action on a list
            a <- sequence a'

            -- convert lits of chars to list of ints
            let aNums = map digitToInt a

            -- sum
            let acc = sum aNums

            -- report
            putStr "The total is "
            putStrLn (show acc)

            return ()

-- 10.10.6 --
getLine' :: IO String
getLine' = do x <- getChar
              if x=='\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

{- use `getCh` to define readLine that
behaves mostly like getLine but it lets you
use the delete key -}

getCh :: IO Char
-- the action getCh reads a single character from the keyboard
-- without echoing it to the screen.
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do x <- getCh
              if x=='\n' then
                return []
              else
                if x=='\DEL' then
                  do putChar '\b'
                     xs <- readLine
                     return xs
                else
                  do putChar x
                     xs <- readLine
                     return (x:xs)
