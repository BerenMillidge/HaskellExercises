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
addVectors a b = (fst a + fst b, snd a + snd b)

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