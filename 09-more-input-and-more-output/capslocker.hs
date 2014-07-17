import Control.Monad
import Data.Char

-- call it like
-- ./capslocker < haiku.txt
-- to perform the input redirection
main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l
