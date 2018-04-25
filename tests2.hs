lucky:: (Integral a) => a -> String
lucky 7 = "Correct!"
lucky x = "Sorry, incorrect!"

doubleThis x y = x + y

bib x = x*x

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2,y2) = (x1+x2, y1+y2)
--addVectors a b = (fst a + fst b, snd a + snd b)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

-- recursive length function - in functoinal programming, bascially always use recursion instead of loops!

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi  <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise  = "Normal!"
    where bmi = weight/height ^ 2       

max' :: (Ord a) => a -> a -> a
max' a b
     | a> b = a
     | otherwise = b

myCompare:: (Ord a) => a -> a -> Ordering
myCompare a b
| a > b = GT
| a == b = EQ
| otherwise = LT

initials:: String -> String -> String
initial firstname lastname = [f] ++ "." ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

cyclinder:: (RealFloat a) => a -> a -> a
cyclinder r h = 
	let sideArea = 2 * pi * r * h
		topArea = pi* r^2
	in sideArea + 2 * topArea

calcBmis:: (RealFloat a) =. [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi>25.0]

describeList:: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
												[x] -> "A singleton list"
												xs -> "a longer list"