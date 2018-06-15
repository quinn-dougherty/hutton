import System.IO
import Data.Char

{-act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)
-} 
strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

-- hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             playHangman word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

playHangman :: String -> IO ()
playHangman word = do putStr "? "
                      guess <- getLine
                      if guess == word then
                        putStrLn "You got it!!"
                        else
                        do putStrLn (match word guess)
                           playHangman word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- Nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = reverse [1..5]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                       else
                       do putStrLn "ERROR: invalid digit"
                          getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player =
  do newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!!"
     else
       do newline
          putStr "Player "
          putStrLn (show player)
          row <- getDigit "Enter a row number: "
          num <- getDigit "Stars to remove : "
          if valid board row num then
             play (move board row num) (next player)
          else
             do newline
                putStrLn "ERROR: Invalid move"
                play board player

nim :: IO ()
nim = play initial 1

-- shell in emacs doesn't do IO monad correctly. terminal does outside in debian.


