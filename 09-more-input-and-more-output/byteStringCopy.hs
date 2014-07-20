import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = do
    (sourceFilename:destinationFilename:_) <- getArgs
    copyFile sourceFilename destinationFilename

copy :: FilePath -> FilePath -> IO ()
copy source dest = do
    sourceContents <- B.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle sourceContents
            hClose tempHandle
            renameFile tempName dest)
-- ./byteStringCopy bart.txt bort.txt
