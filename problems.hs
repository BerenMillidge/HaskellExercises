-- examples from the functinoally solving problems section!

import Data.List

-- so break down the expression into a list of words, and then apply a fold to solvethe problem as the stack
-- so it works reasonably
-- just applies pattern matching toget all the reasonable operators you want and then appends it to the list
-- its quite simply actually, although a slightly different way of thinking about it
-- it is impressive how you just effectively write it in terms of what you want the cmoputatoin to be

solveRPN :: (Num a, Read a) => String -> Float
solveRPN expression = head (foldl foldingFunction [] (words expression))
	where foldingFunction (x:y:ys) "*" = (x*y) : ys
		  foldingFunction (x:y:ys) "+" = (x+y):ys
		  foldingFunction (x:y:ys) "-" = (x-y): ys
		  foldingFunction (x:y:ys) "/" = (x/y):ys
		  foldingFunction (x:y:ys) "^" = (x**y):ys
		  foldingFunction (x: xs) "log" = log x:xs
		  foldingFunction (x:xs) "sin" = sin x: xs
		  foldingFunction xs "sum" = [sum xs]
		  foldingFunction xs "product" = [product xs]
		  foldingFunction xs numberString = read numberString:xs


--define own product function just to make sure it exists and to experiment
-- hopefully this sort of thing would work!
product :: (Num a) => [a] -> a
product xs = foldl f 1 xs
	where f acc (x:xs) = acc*x:xs