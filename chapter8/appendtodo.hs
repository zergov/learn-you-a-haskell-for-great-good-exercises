import System.IO

main = do
  todo <- getLine
  appendFile "todos.txt" (todo ++ "\n")
