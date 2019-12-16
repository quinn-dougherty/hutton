-- |

module Utils where
import Data.Char
import System.IO

getCh :: IO Char
-- the action getCh reads a single character from the keyboard
-- without echoing it to the screen.
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: (Int, Int) -> String -> IO ()
writeat p xs = do goto p
                  putStr xs
