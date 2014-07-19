import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
    (command:argList) <- getArgs
    dispatch command argList

--
-- Take a command and return the associated function
--
dispatch :: String -> [String] -> IO ()
dispatch "add" = add

--
-- Add a To-Do task
--
add :: [String] -> IO ()
add [filename, toDo] = appendFile filename (toDo ++ "\n")
-- ./todo add todo.txt "Find the magic sword of power"

