import Data.Char

-- What bind `>>=` does
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- gossage avec do
foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

------------------------------------------------------
oddLengthOnly :: String -> Maybe String
oddLengthOnly s
  | odd $ length s = Just s
  | otherwise = Nothing

justFirstLetter :: String -> Maybe Char
justFirstLetter "" = Nothing
justFirstLetter (x:xs) = return x
-- with this, we can use `>>=` with oddLengthOnly and justFirstLetter
-- I.E:
-- ghci> oddLengthOnly "zomething strange" >>= justFirstLetter
-- ghci> Just 'z'
--------------------------------------

-- Some other stuff
fs :: Int -> Maybe String
fs x = Just (show x)

f :: Int -> Maybe Int
f x = Just (x + 1)

fadd :: Int -> Int -> Maybe Int
fadd n x = Just (n + x)

frepeat :: Int -> a ->  Maybe [a]
frepeat x a = Just (take x . repeat $ a)

