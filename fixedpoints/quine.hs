data Quine a = IO String

f :: Int -> Int
f x = 2 ^ (0 - x)
{-
quinef :: Quine (Int -> Int)
quinef = show quine
-}

quinef :: a -> IO ()
quinef f = 

--harder than i thought. 
