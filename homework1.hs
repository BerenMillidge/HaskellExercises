parseStringToIntList:: String -> [Integer]
parseStringToIntList s[x:xs] = x : parseStringToIntList xs

toDigits:: Integer -> [Integer]
toDigits 0 = []
toDigits -x = []
toDigits x = parseStringToIntList show x


toDigitsRev:: Integer -> [Integer]
toDigitsRev x = reverse toDigits x

doubleEveryOther:: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x1,x2:xs) = x1, x2*2: doubleEveryOther xs

sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = x + sumDigits xs

validate:: Integer -> Bool
validate x 
	| res `mod` 10 ==0 = True
	| otherwise = False
	where res = sumDigits (toDigitsRev x)


--towers of hanoi exercise
type Peg = String
type Move = (Peg, Peg)

hanoi:: Integer -> Peg -> Peg -> Peg -> [Move]