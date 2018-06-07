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


-- okay, now it's time to recursive data structures.. let's see how it's going!
-- lists can be defined recursively
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- first let's implement a binary search tree!
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--creation function

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x -- pattern match to create a new tree if nothing already existing!
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x ==a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

-- so yeah, algebraic and recursive datastructures are powerful. Similarly, you can make your own typeclasses
-- to be whatever you want, so let's do this!

--typeclasses define the kind of behaviour a type is expected (required) to have. for instance Eq
-- typeclass - all types implementing Eq have to be able to have equals and non equals functions
--i.e.:
class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	x == y = not (x \=y)
	x \= y = not (x ==y)

--the functions are defned in terms of mutual recursion which is fundortunate - and presumably an infinite loop
-- however, you can specifiy specific instances generally!

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow = Yellow = True
	_ == _ = False

-- and implement show also
instance Show TrafficLight where
	show Red = "Red Light"
	show Yellow = "Yellow light"  
    show Green = "Green light"

 -- typeclasses can subclass other type classes - i.e.
--Class (Eq a) => Num a where 
	--...
-- so what about creating javascript style boolean assumptions - i.e. yesno for all sorts of random things
-- like "" = False, and "0" = False etc
--define typeclass yesNo

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id

instance YesNo (Maybe a) where
	yesno (Just _) = True
	yesno Nothing = False

instance YesNo (Tree a) where
	yesno EmptyTree = False
	yesno _ =  True

--now the true fun begins -- functors!
-- the functor typeclass is basically for things that can be mapped over so, how is it implemented

class Functor f where
	fmap :: (a->b) -> f a -> f b
-- crucially here f is confusingly NOT a function application, rather it is an algebraic type constructor 
-- for instance like Maybe a - but it can be for ANY type constructor. It can be for functions
-- but it doesn't have to be, which is why it's confusing here it's called f 
-- but essentially - a functor is a typeclass which takes ANY concrete or constructed type
-- and a function mappping that type to ANY other concrete or constructed type, and then performs the mapping
-- to get any concrete or constructed result type... makes sense! if very abstract 

	-- map is implemented simply
instance Functor [] where
	fmap = map

-- but can implement a functor functionality for Maybe, for instance
instance Functor Maybe where
	fmap f (Just x) = Just (f x)
	fmap f Nothing = Nothing

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- this functor is then impleented completely recursively, which is really cool to see
-- although it presumes the function f only has/needs access to local subbranch tree information!

instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x


--types also have types, which are called 'kinds'. * is the kind/type of a concrete type
-- other complex typeclasses and the like are functions of kinds - i.e. types of types
-- so for instance Maybe is of kind *-> * as it takes a concrete type and returns another concrete type
-- functors are (*->*)->*->*
-- since they take a type constructor and a concrete type and return another concrete type
-- so that's cool, and very abstract and makes a lot of sense!