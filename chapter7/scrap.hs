import Data.List

hasConsecutive :: String -> Bool
hasConsecutive = (== 1) . length . filter predicate . group
  where predicate g = length g > 1

