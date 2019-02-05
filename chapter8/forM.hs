import Control.Monad

main = do
  colors <- forM [1,2,3,4] (\n -> do
    putStrLn $ "What color do you associate with:" ++ show n ++ "?"
    color <- getLine
    return color)
  putStrLn "The color associated with 1, 2, 3 and 4 are:"
  mapM putStrLn colors
