-- Previously located at Control.Monad.Instances
-- Nowadays in GHC.Base so no need for import ....

--
-- addStuffV1
--
addStuffV1 :: Int -> Int
addStuffV1 = do
    a <- (*2)
    b <- (+10)
    return (a+b)
-- addStuffV1 4 --> 22

--
-- addStuffV2
--
addStuffV2 :: Int -> Int
addStuffV2 x = let
    a = (*2) x
    b = (+10) x
    in a+b
-- addStuffV2 4 --> 22

