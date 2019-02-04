import Data.List
import Data.Char
import qualified Data.Map as Map

-- nub takes a list an strips duplicates
-- nub ['a','b','c','a']
-- >> ['a','b','c']
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- we will make a function that takes two lists and tells us if
-- the first list is wholly contained anywhere in the second list.

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle = any (isPrefixOf needle) . tails

encode :: Int -> String -> String
encode offset = map (\c -> chr $ offset + ord c)

-- decode :: Int -> String -> String
-- decode offset = map (\c -> chr $ ord c - offset)
-- facon du livre vraiment swag
decode :: Int -> String -> String
decode offset = encode (negate offset)

-- what’s the first natural number such that the sum of its digits equals 40?
sumNumDigits = sum . map digitToInt . show

firstTo40 = find (\x -> (sumNumDigits x) == 40) [1..]
firstTo n = find (\x -> (sumNumDigits x) == n) [1..]

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]
