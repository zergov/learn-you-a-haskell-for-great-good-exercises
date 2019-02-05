import System.IO

todosFile :: String
todosFile = "todos.txt"

main = do
  content <- readFile todosFile
  let tasks = zipWith (\n line -> show n ++ ", " ++ line) [0..] (lines content)
  mapM_ putStrLn tasks
