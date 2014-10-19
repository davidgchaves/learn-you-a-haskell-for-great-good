import System.Random
import Control.Monad.State

-- Wrapping the random function from System.Random
-- with the State Monad
randomState :: (RandomGen g, Random a) => State g a
randomState = state random

-- True:  Tails
-- False: Heads
threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomState
    b <- randomState
    c <- randomState
    return (a, b, c)
-- runState threeCoins (mkStdGen 33) --> ((True,False,True), 680029187 2103410263)
