import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

main = do
    (command:argList) <- getArgs
    dispatch command argList

--
-- Take a command and return the associated function
--
dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesNotExist command

--
-- Add a To-Do task
--
add :: [String] -> IO ()
add [filename, toDo] = appendFile filename (toDo ++ "\n")
-- ./todo add todo.txt "Find the magic sword of power"
add _ = putStrLn "The add command takes exactly two arguments"
-- ./todo add todo.txt "Find the magic sword of power" "and some more"

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

--
-- Remove a To-Do task
--
remove :: [String] -> IO ()
remove [filename, taskNumberStr] = do
    toDos <- readFile filename
    let toDoTasks = lines toDos
        taskNumberToRemove = read taskNumberStr
        newToDos = unlines $ delete (toDoTasks !! taskNumberToRemove) toDoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newToDos
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)
-- ./todo remove todo.txt 1

--
-- The command does not exist
--
doesNotExist :: String -> [String] -> IO ()
doesNotExist command _ = putStrLn $ "The " ++ command ++ " command does not exist"
-- ./todo remove-again blah blah blah

