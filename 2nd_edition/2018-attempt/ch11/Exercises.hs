module Exercises where

import GameUtils
import Tictactoe
import GameTree

-- 1. Verify counting problem
verify :: Int
verify = 549946
-- "there are 549946nodes in the complete game tree for a 3x3 tictactoe game
{-
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]
-}



