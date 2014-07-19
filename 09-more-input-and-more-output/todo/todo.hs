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
dispatch "view" = view

--
-- Add a To-Do task
--
add :: [String] -> IO ()
add [filename, toDo] = appendFile filename (toDo ++ "\n")
-- ./todo add todo.txt "Find the magic sword of power"

--
-- Display current To-Do tasks
--
view :: [String] -> IO ()
view [filename] = do
    toDos <- readFile filename
    let toDoTasks = lines toDos
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task)
                                [0..]
                                toDoTasks
    putStr $ unlines numberedTasks
-- ./todo view todo.txt

