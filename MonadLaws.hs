-- Demonstration of the monad laws with the List Monad

-- The first monad law states that if we take a value,
-- put it in a default con- text with return, and then feed it to a function by using >>=,
-- that’s the same as just taking the value and applying the function to it.
-- To put it formally, return x >>= f is the same damn thing as f x.
leftIdentity = do
  putStrLn $ show $ return [3] >>= f
  putStrLn $ show $ f [3]
    where f x = [x,x,x]

-- The second law states that if we have a monadic value and we use >>= to feed it to return,
-- the result is our original monadic value. Formally, m >>= return is no different than just m.
rightIdentity = do
  putStrLn $ show $ m >>= (return)
  putStrLn $ show $ m
    where m = [1337, 420]

-- The final monad law says that when we have a chain of monadic function applications with >>=,
-- it shouldn’t matter how they’re nested.
-- Formally written, doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g).
associativity = do
  putStrLn $ show $ (m >>= f) >>= g
  putStrLn $ show $ m >>= (\x -> f x >>= g)
    where m = [1337, 420]
          f x = map (show . (*100)) [x]
          g x = reverse [x]
