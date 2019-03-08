fs :: Int -> Maybe String
fs x = Just (show x)

f :: Int -> Maybe Int
f x = Just (x + 1)

fadd :: Int -> Int -> Maybe Int
fadd n x = Just (n + x)

frepeat :: Int -> a ->  Maybe [a]
frepeat x a = Just (take x . repeat $ a)
