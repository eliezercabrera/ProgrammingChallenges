import System.Environment
import System.Random.MWC
import Control.Monad

main = do
    [n] <- fmap read <$> getArgs
    gen <- createSystemRandom
    prices <- replicateM n (uniformR (5.0, 200.0) gen) :: IO [Float]
    putStr . unwords . fmap show $ prices