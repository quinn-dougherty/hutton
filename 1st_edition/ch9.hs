--import System.IO

--derived primitives
{-
getLine' :: IO String
getLine' = do x <- getChar
             if x == '\n' then
                return []
             else
               do xs <- getLine'
                  return (x:xs)
-} 
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strLen :: IO ()
strLen = do putStr' "Enter a string: "
            xs <- getLine
            putStr' "The string has "
            putStr' (show (length xs))
            putStrLn " characters"

-- IO utils
beep :: IO ()
beep = putStr "\BEL"
cls :: IO ()
cls = putStr "\ESC[2J"

goTo :: Pos -> IO ()
goTo (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goTo p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as
{-
seqn' [] = return []
seqn' (act:acts) = do x <- act
                      xs <- seqn' acts
                      return (x:xs)
-}

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

putStr'' xs = seqn [putChar x | x <- xs]

-- Game of life, pg 112
type Pos = (Int, Int)

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


width :: Int
width = 5
height :: Int
height = 5

-- hopefully we could do terminal-like graphics? 
showUniverse :: Int -> Int -> [IO ()]
showUniverse w h = take h (repeat (seqn [writeAt (x,y) "." | x <- [1..w], y <- repeat 0])) -- hrmph. 
showCells :: Board -> IO ()
showCells b = seqn [writeAt p "0" | p <- b]
-- here we need to have some idea of universe/ambient and complement, in order for it to look satisfying... 
isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = ((mod (x-1) width) + 1, (mod (y-1) height) + 1)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y)  ,(x-1,y+1),
                          (x,y+1)  ,(x+1,y+1)]

liveNeighbs :: Board -> Pos -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbs b p) [2,3]]

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x:rmDups (filter (/=x) xs)
births :: Board -> [Pos]
births b = [p | p <- rmDups (concat (map neighbs b)),
                isEmpty b p,
                liveNeighbs b p == 3]

nextGen :: Board -> Board
nextGen b = survivors b ++ births b


life :: Board -> IO()
life b = do cls
            showCells b
            wait 5000
            life (nextGen b)
-- not with graphics, here.
