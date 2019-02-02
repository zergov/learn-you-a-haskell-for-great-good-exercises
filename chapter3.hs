factorial :: Integer -> Integer
factorial n = product [1..n]

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

vadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vadd (a,b) (c, d) = (a+c, b+d)

first :: (a,b,c) -> a
first (a, _, _) = a

second :: (a,b,c) -> b
second (_, b, _) = b

third :: (a,b,c) -> c
third (_, _, c) = c

-- Try to use testtail with empty list: []
tail' :: [a] -> [a]
tail' [] = error "Cannot give the tail of an empty list"
tail' (x:xs) = xs

head' :: [a] -> a
head' [] = error "Cannot give the head of an empty list"
head' (x:_) = x

firstletter :: String -> String
firstletter "" = "Oops, this is an empty string!"
firstletter s@(x:xs) = "The first letter of: " ++ s ++ " is " ++ [x]

bmi :: Double -> String
bmi n
  | n <= 18.5 = "You're super slim!"
  | n <= 25 = "You're normal!"
  | n <= 35 = "You're starting to get fat!"
  | otherwise = "You're a whale!"


max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | b > a = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b      -- Defining a function with infix
  | a > b = GT
  | a == b = EQ
  | b > a = LT


bmi' :: Double -> Double -> String
bmi' weight height
  | bmi <= skinny = "you're slim!"
  | bmi <= normal = "you're normal!"
  | bmi <= fat = "you're fat!"
  | otherwise = "You're a whale"
    where
      bmi = weight / height ^ 2
      skinny = 18.5
      normal = 25
      fat = 35

-- pattern match in where clauses
initials :: String -> String -> String
initials firstname lastname = f : "." ++ [l]
  where
    (f:_) = firstname
    (l:_) = lastname

-- also pattern matched directly in params
initials' :: String -> String -> String
initials' (f:_) (l:_) = f : "." ++ [l]
