cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)
type Board = [Pos]

width :: Int
width = 10

height :: Int
height = 10

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1),
                          (x+1, y-1), (x-1, y),
                          (x+1, y), (x-1, y+1),
                          (x, y+1), (x+1, y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

births' :: Board -> [Pos]
births' b = [(x,y) | x <- [1..width],
                     y <- [1..height],
                     isEmpty b (x,y),
                     liveneighbs b (x,y) == 3]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)
