import Data.Word
import Data.List
import System.Environment

collatzNext :: Word32 -> Word32
collatzNext a = (if even a then a else 3*a+1) `div` 2

-- new code
collatzLen :: Word32 -> Int
collatzLen a0 = lenIterWhile collatzNext (/= 1) a0

lenIterWhile :: (a -> a) -> (a -> Bool) -> a -> Int
lenIterWhile next notDone start = len start 0 where
    len n m = if notDone n
                then len (next n) (m+1)
                else m
-- End of new code

main = do
    [a0] <- getArgs
    let max_a0 = (read a0)::Word32
    print $ maximum $ map (\a0 -> (collatzLen a0, a0)) [1..max_a0]
