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

--boardOfLengthN :: Int -> Board 
{-
putBoard' :: Board -> Int -> IO ()
putBoard' b n = do [putRow k a | k <- n, a <- b]
-}
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

-- LIFE 
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10
height :: Int
height = 10

type BoardLife = [Pos]

glider :: BoardLife
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: BoardLife -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: BoardLife -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: BoardLife -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x-1,y+1), (x,y+1)]

liveneighbs :: BoardLife -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: BoardLife -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

births :: BoardLife -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: BoardLife -> BoardLife
nextgen b = survivors b ++ births b

life :: BoardLife -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
                     

-- exercises
--10.10.1
putStr' :: String -> IO ()
putStr' s = sequence_ [putChar c | c <- s]

--10.10.2 -- modify nim to put n rows on. 

