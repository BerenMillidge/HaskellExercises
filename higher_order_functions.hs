-- okay, higher order functions - a key part of haskell, and any functional programming language
-- whatsoever!

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

--let multTwo = multThree 9 

applyTwice :: (a-> a) -> a -> a
applyTwice f x = f (f x)

zipwith :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

flip :: (a-> b -> c) -> (b -> a -> c)
flip f = g
	where g x y = f y x

-- this is a common higher order function functional programming - map!
-- obviously implemented recursively. Function simply takes an element out of the list
-- applies function to the element and makes a list with it and amp applied to the rest of the list
map :: (a-> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

--here's another one I know and love from javascript!
-- it's cool seeing their original inspirations in haskell!
filter:: (a -> Bool) -> [a] -> [b]
filter _ [] = []
filter p (x:xs)
	| p x = x: filter p xs
	| otherwise = filter p xs

--just removes ignores those elements and doesn't add to retunr list if the filter is not true
-- find the largest number that is divisible bby out argument
--simple with filter!
largestDivisible:: (Integral a) => a-> a -> a
largestDivisible f num divider = head (filter p [num, num-1..])
	where p num = nun `mod` divider == 0

takeWhile:: (a-> Bool) -> a -> a
takeWhile pred (x:xs)
	| p x = x: takeWhile p xs
	|otherwise


--time to have some fun with collatz sequences
collatzChain:: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n = n:chain (n `div` 2)
	| odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains 0 = 0
numLongChains = length (filter (\xs -> length xs > 15) (map chain[1.100]))