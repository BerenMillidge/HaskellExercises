-- okay, doing randomness in haskell briefly!

import System.Random

-- so the main function is random, which gives back a random generator ANd a number, and it creates 
-- a new generator each time, thus allowing chains of randomness eventhoguh the function is entirely
-- deterministic given the generator
--i.e.
random :: (RandomGen g, Random a) => g -> (a, g)

-- can make a standard generator using the mktdgen function - it has type
mkStdGen :: Int -> StdGen

--we can combine this into multiple functinos
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
	let (firstCoin, newGen) = rendom gen
		(secondCoin, newGen') = random newGen
		(thirdCoin, newGen'') = random newGen'
		in (firstCoin, secondCoin, thirdCoin)

-- but what if you don't want to hardcode but have it as a potentially infinite stream, then a function like
	randoms' :: (RandomGen g, Random a) => g -> [a]
	randoms' gen = let (value, newGen) = random gen in value: randoms' newGen

	