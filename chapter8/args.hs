import System.Environment

main = do
  progName <- getProgName
  arg <- getArgs

  putStrLn $ "The program name: " ++ progName
  putStrLn ""
  putStrLn "The arguments are:"
  mapM_ putStrLn $ zipWith (\n a -> "arg" ++ show n ++ ": " ++ a ) [1..] arg
