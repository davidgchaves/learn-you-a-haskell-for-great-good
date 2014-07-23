import Data.Char
import Data.List

--
-- Multiple function composition using fmap
--

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
-- runhaskell functionCompositionIO.hs
--      hello there
--      --> E-R-E-H-T- -O-L-L-E-H
