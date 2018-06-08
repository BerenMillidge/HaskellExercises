--todo haskell task!

import System.Environment
import System.Directory
import System.IO
import Data.List

-- so dispatch dispatches the actions - has commandline arguments as keys and then has io functions
-- that dispatchesthe actions as required - this is the main decisional core of the program

dispatch:: [(String, [String] -> IO () )]
dispatch = [ ("add", add),
			("view", view),
			("remove", remove)
			]

--now just have to define these things again

-- now implement the actual functions properly
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


--yeah, haskell functions are very concise, and kind of difficult to understand
-- it is a slightly different way of thinking about it all, which makes sense!
view :: [String] -> IO ()
view [filename] = do
	contents <- readFile fileName
	let todoTasks = lines contents
		numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
	putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
	handle <- openFile fileName ReadMode
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let number = read numberString
		todoTasks = lines contents
		newTodoItems = delete (todoTasks !! number) todoTasks
	hputStr tempHandle $ unlines newTodoItems
	hclose handle
	hclose tempHandle
	removeFile fileName
	renameFile tempName fileName

main = do
	(command:args) <- getArgs
	let (Just action) = lookup command dispatch
	action args