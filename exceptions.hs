-- okay, haskell also has exceptions - can arise in either pure code or IO code
-- usually only use exceptions for IO code interfacing with the outside world, since exceptions 
-- massively compliacte the reasoning of pure functions - isntead use types like MAybe in pure functions
-- so the compelxity can be offloaded back into the type system which it is defined for! - only
-- try to use exceptions where necessary in the IO code part - althoguh to be honest, a little bit of exception
-- offloading may help instead of using maybe all the time and constant nothing checks in every pure function
-- who knows though!?

import System.Environment
import System.IO

main = do 
	(fileName: _ ) <- getArgs
	contents <- readFile fileName
	putStrLn $ "Thefile has " ++ show (length (lines contents)) ++ " lines!"

-- with an if condition checking for nonexistence

main = do 
	(fileName: _ ) <- getArgs
	fileExists <- doesFileExist filename
	if fileExists
		then do contents <- readFile fileName
				putStrLn $ "Thefile has " ++ show (length (lines contents)) ++ " lines!"
		else do putStrLn "The file doesn't exist!"
		
--now with exceptions
-- use exceptions using the catch function of type
catch' :: IO a -> (IOError -> IO a) -> IO a
-- it takes two parametes first is an IO action, the second is the handler - if the first IO action passed
	-- throws an exception, that exception gets passed to handler, which decides what to do
		-- and then the handler returns a final IO action that tells us what to do with respect
		-- to the exception!

import System.IO.Error
--now with exceptions!

main = toTry `catch` handler

toTry :: IO ()
toTry = do
	(fileName: _) <- getArgs
	contents <- readFile fileName
	putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Error occured!"

--this handler catches everything though, so yeah, ty a better handler
handler' :: IOError -> Io ()
handler' e
	| isDoesNotExistError e = putStrLn "the file doesn't exist!"
	| otherwise = ioError e

-- can also use some of the information in the error message
handler'' :: IOError -> IO ()     
handler'' e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e   
	