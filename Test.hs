module Test where

import Data.List

score :: [Int] -> [(Int, Int, Int)]
score = scanr count (1, 0, 0) . reverse

--count :: Int -> (Int, Int) -> (Int, Int)
count n (lvl, ls, acc) = (lvl', ls + n, acc + score ())
  where lvl' = ls `div` 10 + 1
        score ()
          | n == 0 = 0
          | n == 1 = 40   * lvl'
          | n == 2 = 100  * lvl'
          | n == 3 = 300  * lvl'
          | n == 4 = 1200 * lvl'
          | otherwise = error $ "Invalid line count: "++show n
