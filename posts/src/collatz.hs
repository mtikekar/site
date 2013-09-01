import Data.Word
import Data.List
import System.Environment

collatzNext :: Word32 -> Word32
collatzNext a = (if even a then a else 3*a+1) `div` 2

collatzLen :: Word32 -> Int
collatzLen a0 = length $ takeWhile (/= 1) $ iterate collatzNext a0

main = do
    [a0] <- getArgs
    let max_a0 = (read a0)::Word32
    print $ maximum $ map (\a0 -> (collatzLen a0, a0)) [1..max_a0]
