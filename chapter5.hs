zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs


-- As another example, let’s find the largest num- ber under 100,000
-- that’s divisible by 3,829.
-- To do that, we’ll just filter a set of possibilities in which
-- we know the solution lies:
largestNumberExercise = (head . filter p) [100000, 99999..0]
  where
    p n = n `mod` 3829 == 0

sumofoddsquare = (sum . takeWhile (<10000) . filter odd . map (^2)) [1..]


-- Start with any natural number.
-- • If the number is 1, stop.
-- • If the number is even, divide it by 2.
-- • If the number is odd, multiply it by 3 and add 1.
-- • Repeat the algorithm with the resulting number.
--
-- For all starting numbers between 1 and 100,
-- how many Collatz chains have a length greater than 15?

chain' :: Integer -> [Integer]
chain' 1 = [1]
chain' n
  | even n = n : chain' (n `div` 2)
  | odd n = n : chain' (n * 3 + 1)

collatzExercise = (length . filter p . map chain') [1..100]
  where
    p chain = length chain < 15

-- folds
sum' :: [Int] -> Int
sum' = foldl (+) 0

-- showcase: x : xs is way faster than [x] ++ xs
-- mapl' (*2) [1..100000] ===> slow
-- mapr' (*2) [1..100000] ===> faster
mapl' :: (a -> b) -> [a] -> [b]
mapl' f xs = foldl (\ acc x -> acc ++ [f x]) [] xs

mapr' :: (a -> b) -> [a] -> [b]
mapr' f xs = foldr (\ x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = foldr (\x acc -> if e == x then True else acc) False xs

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []

product' :: [Integer] -> Integer
product' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x : acc else acc) []

-- How many elements does it take for the sum of the square roots of all
-- natural numbers to EXCEED 1,000?
sqrtSum :: Int
sqrtSum = 1 + (length . takeWhile (< 1000) . scanl1 (+) . map sqrt) [1..]

-- exercise, rewrite this:
compositionExercise = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
compositionExercise' = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

pfreef x = ceiling (negate (tan (cos (max 50 x))))
pfreef'  = ceiling . negate . tan . cos . max 50

sumofoddsquare' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
