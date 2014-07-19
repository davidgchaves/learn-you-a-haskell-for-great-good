import System.IO
import System.Directory
import Data.List

main = do
    toDos <- readFile "todo.txt"
    let toDoTasks = lines toDos
        numberedTasks = zipWith (\n task -> show n ++ " - " ++ task)
                                [0..]
                                toDoTasks

    putStrLn "These are your To-Do items:"
    mapM_ putStrLn numberedTasks

    putStrLn "Which one do you want to delete?"
    taskNumberStr <- getLine
    let taskNumber = read taskNumberStr
        newToDos = unlines $ delete (toDoTasks !! taskNumber) toDoTasks

    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newToDos
    hClose tempHandle

    removeFile "todo.txt"
    renameFile tempName "todo.txt"
