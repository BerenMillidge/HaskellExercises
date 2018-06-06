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
	| otherwise x


--time to have some fun with collatz sequences
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n = n:chain (n `div` 2)
	| odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains 0 = 0
numLongChains = length (filter (\xs -> length xs > 15) (map chain[1.100]))

--okay, starting with folds now
-- a fold takes a binary function, a starting value, and a list, binary function takes two parameters
-- binary function caled with the accumulator and next element to produce a new accumulator and so forth
-- i.e. simplest case is summing a list
-- foldl folds from the left side, foldr from the right

foldSum :: (Num a) => [a] -> a
foldSum xs = foldl (\acc x -> acc + x) 0 xs

-- it's obvious how it works and how it returns
-- folds return the accumulator as their return type
elemFold :: (Eq a) => a -> [a] -> Bool
elemFold y ys = foldl (\acc x -> if x==y then True else acc) False ys

--map can be implemented with folds

foldMap :: (a->b) -> [a] -> [b]
foldMap f xs = foldr (\x acc -> f x : acc) [] xs

-- could have done foldmap left - i.e. foldl (\acc x -> acc ++ [f x]) [] xs - but apparently ++ is more 
-- cmoputationall expensive than : so use a right fold when building up a list from new list
-- another difference is right folds work on infinite lists while left ones dont!

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
      
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
      
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
      
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
      
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
      
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

-- next thing to learn is the function aplication function $
--basically is function application with really low precedence to stop writing parentheses

($)' :: (a->b) -> a -> b
f $ x = f x

--then there's also the function compositio nfunction .
(.) :: (b-> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
--i.e. it takes two functions and returns a function that is the result of those two functions!
-- i.e..
map $ negage . abs [-3, -1 ...]