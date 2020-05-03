module Timer where

data Timer = Timer {initial :: Float, remaining :: Float} deriving (Eq)

instance Show Timer where
  show t = "T+" <> i <> "/" <> r
    where i = show $ initial t
          r = show $ remaining t



mkTimer :: Float -> Timer
mkTimer t = Timer t' t'
  where t' = abs t

isElapsed :: Timer -> Bool
isElapsed = (<=0) . remaining

resetTimer :: Timer -> Timer
resetTimer = mkTimer . initial

updateTimer :: Float -> Timer -> Timer
updateTimer δ t = t {remaining = subtract δ $ remaining t}
