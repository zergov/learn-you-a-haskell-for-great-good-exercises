fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- standard function re-implemented

-- different from the book
max' :: (Ord a) => [a] -> a
max' [] = error "empty list :("
max' [x] = x
max' (x:y:xs)
  | x > y = max' (x : xs)
  | y > x = max' (y : xs)

-- my attempt
-- replicate' :: Int -> a -> [a]
-- replicate' 0 _ = []
-- replicate' n x = x : replicate' (n-1) x
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

-- sorting
-- quicksort' :: (Ord a) => [a] -> [a]
-- quicksort' [] = []
-- quicksort' (x:xs) = left ++ [x] ++ right
  -- where
    -- left = quicksort' [i | i <- xs, i <= x]
    -- right = quicksort' [i | i <- xs, i > x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = left xs ++ [x] ++ right xs
  where
    left = quicksort' . filter (<= x)
    right = quicksort' . filter (> x)
