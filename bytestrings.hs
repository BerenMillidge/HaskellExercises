-- haskell also has bytestrings, which are more efficient than normal lists/string!
-- so they can deal with large file sizes 
-- two types of bytesting - strict and alzy! - strict bytstrings are
import qualified Data.ByteString as S-- strict
import qualified Data.BYteString.Lazy as B-- lazy obviously!

-- lazy bytestrings are still less likely than lists - lists have a thunk (i.e. promise in JS) 
-- between each element(!) while bytestrings only do so after each chunk of 64K bytes!

-- try to make a more efficient file cpying thing
import System.Environment


main = do
	(fileName1: fileName2:_) <- getArgs
	copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
	contents <- B.readFile source
	B.writeFile dest contents

-- wll generally improve the performance of large string transfers if necessary/useful!