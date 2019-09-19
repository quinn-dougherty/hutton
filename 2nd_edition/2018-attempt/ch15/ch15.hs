--ch15 Lazy Evaluation

--EXERCISE 6: NEWTON'S SQUARE ROOT
epsilon = 0.00001

enough :: Float -> Float -> Bool
enough v y = abs (v-(y^2)) < epsilon

next :: Float -> Float -> Float
next x a = (a + x/a) / 2

sqrtsList :: Float -> [Float]
sqrtsList = \x -> takeWhile (\w -> not (enough x w)) (iterate (next x) (1 + epsilon) )

sqroot :: Float -> Float
sqroot x = (head . reverse) (sqrtsList x)
