-- okay, this is the fun bit where I get to make my own types within haskell!

-- can use data keywork to define possible values
data Bool = False | True

--can define types in terms of other types - i.e. 'tuple types'
data Circle = Circle Float Float Float
data Rectangle = Rectangle Float Float Float Float
data Shape = Circle | Rectangle deriving (Show)

--can define own functions using own types
--use pattern matching to deal with this
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 -x1) * (abs $ y2 - y1)

--can also add more things
data Point = Point Float Float deriving (Show)

--nudge -i.e. translate function for shape

nudge:: Shape -> Float -> Float -> Shape
nudge (Cricle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0 ) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point (0 0)) (Point w h)

-- obviously can use this to derive record types as well!
-- has a special syntax which effectively gives you getters and setters on the attributes
--so it uses standard c-derived struct syntax for this!
data Person = Person {
	firstName :: String
	, lastName :: String
	, age :: Int
	, height:: Float
	, phoneNumber :: String
	, flavor :: String
} deriving (Eq, Read, Show)

-- if doing it in thus record/struct fashin haskell automatically creates getter and setter functions for you!

	-- type constructors can take other types as input parameters to allow for meta types
	-- obvious example is the maybe type
data Maybe a = Nothing | Just a

	-- deriving a parametrised vector type with operations
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n) 

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

dotProduct :: (Num t) => Vector t -> Vector t -> t
dotProduct (Vector i j k) (Vector (l m n) = i*l + j*m + k*n

-- typeclasses define broad classes of behaviour for type s- i.e. a type should have typeclass eq if it can be equated with another of its type
	-- similarly ord if it can be ordered
 -- definable typeclasses are eq ord enum bounded show and read - can be defind automatically
-- use deriving to add these automatical typeclasses

-- enum is for things with a fixed range, fixed points and discrete successors and predecessors
-- read means itcan be read in as a string, show means it can be read out as a string
-- and bounded means it has a possible maximum and minimum value!

-- type synonyms are giving a type a different name so it makes sense, done with the type keyword
type String = [Char]

--can also use multiple different parametrised types - i.e. 
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- for instance left can be an error message, and right can be the otherwise normally returned result of the computation!
