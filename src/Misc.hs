module Misc where

import Data.List (sort)

newtype Range a = Range (a, a) deriving (Show, Eq)

range :: Ord a => a -> a -> Range a
range a b = Range (min a b, max a b)

inRange :: Ord a => a -> Range a -> Bool
inRange x (Range (a, b)) = a <= x && x < b

rangeFromList :: (Ord a, Eq a, Enum a) => [a] -> Maybe (Range a)
rangeFromList xs = if   f xs'
                   then Just $ Range (head xs', last xs') 
                   else Nothing
  where
    f [] = True
    f [_] = True
    f (x:y:zs) = succ x == y && f (y:zs)
    xs' = sort xs
-- 1 : 2 : 3 : 4 : ()

asList :: Enum a => Range a -> [a]
asList (Range (a, b)) = [a..b]


indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap fn = fmap fn . zip [0..]

-- n < 0         = Nothing
-- n > length xs = Nothing
-- otherwise     = n !! xs
nth :: Int -> [a] -> Maybe a
nth n xs = case drop n xs of
            x:_ | n >= 0 -> Just x
            _            -> Nothing

justIf :: a -> Bool -> Maybe a
justIf a b = if b then Just a else Nothing
