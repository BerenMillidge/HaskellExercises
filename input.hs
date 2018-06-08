-- okay, starting to do input and output relations in haskell - i.e. printing to screen and using IO Monads

--hellow world

main = putStrLn "hello world!"
-- it works!

main = do
	putStrLn "Hello, what's your name?"
	name <- getLine -- special syntax for getting data from impure IO monads
	-- this binds the result of the action to the variable name, name = getLine would bind the function - i.e .of type IO (String)
	purStrLn  ("Hey" ++ name ++ " , you rock!")
	--the final variable in a IO do block cannot be bound to a name!

	main = do
		putStrLn "First name?"
		firstName <- getLine
		putStrLn "Last name?"
		lastName <- getLine
		let upperfirst = map toUpper firstName
			uppderlast = map toUpper lastName
		putStrLn $ "hey " ++ upperfirst ++ " " ++ uppderlast


	--prints out lines but reverse

main = do
	line <- getLine
	if null line
		then return ()
	else do
		putStrLn $ reverseWords line
		main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words -- pretty cryptic function definition there!


-- you can also use standard recursion and pattern matching to define your own IO operations
--for instance, putStr can be defined as so
putStr :: String -> IO ()
putStr [] = return () 
putStr (x: xs) = do
	putChar x
	putStr xs

import Control.Monad
main = do
	c <- getChar
	when (c /= ' ') $ do
		putChar c
		main

main = forever $ do
	putStr "Give me some input: " 
	l <- getLine
	putStrLn $ map toUpper l

 
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  


-- haskell also has streams - for instance get contents which reads everything from standard input
-- until it encounters end of file character
-- and streams in haskell are lazily(!) evaluated!


main = do
	contents <- getContents
	putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input = 
	let allLines = lines input
		shortLines = filter (\line -> length line <10) allLines
		results = unlines shortLines
	in result

-- there's also the interact function which does precisely that - takes some input sting, run a function
-- on it, and returns an io action that will take the niput, run the function, and thne print it out

--so can do:
main = interact shortLinesOnly --instead

--can even do it in a single line
main = interact $ unlines . filter ((<10) . length) . lines -- although this is entirely cryptic!

respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") lines contents)
	where isPalindrome xs = xs == reverse xs

--so to get t a file can use openfile, which gives us file handles and file modes
-- IO mode encapsulates the file opening mode, and is effecively defined as 

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- IOhandle type gives us the file handle in a thing!

--crazily enough in haskell you cn actually set low level buffering details
main = do
	withFile "something.txt" ReadMode (\handle -> do
		hSetBuffering handle $ BlockBuffering (Just 2048)
		contents <- hGetContents handle
		putStr contents)

-- this will read the file and print it in 2048 byte blocks!

-- haskell also obviously is able to take commandline arguments into tis functions!
-- two useful functions for this
import System.Environment 
import Data.List

--getArgs :: IO -> [String] - contains a list of all commandline arguemtns
--getProgName :: IO String - is an io action that contains program name!
main = do
	args <- getArgs
	progName <- getProgName
	putStrLn $ "The arguments are: " ++ mapM putStrLn args
	putStrLn $ "The program name is: " ++ progName



--now lets actually use this to create a very minimal haskell app - todo!