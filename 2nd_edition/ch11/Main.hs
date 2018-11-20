module Main where

import System.IO
import Tictactoe
import GameTrees
import GameUtils

--MAIN
run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move"
                           run' g p
                  [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

tictactoe :: IO ()
-- two players, each from commandline input. 
tictactoe = run empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p
play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins! \n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move"
                           play' g p
                  [g'] -> play g' (next p)
  | p == X = do putStr "Player X is thinking... "
                (play $! (bestmove g p)) (next p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O
