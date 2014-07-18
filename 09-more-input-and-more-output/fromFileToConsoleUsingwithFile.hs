import System.IO
import Control.Exception

main = do
    withFile' "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

--
-- Implementing withFile using Control.Exception.bracket
--

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' filename mode f =
    bracket (openFile filename mode)
            (\handle -> hClose handle)
            (\handle -> f handle)
