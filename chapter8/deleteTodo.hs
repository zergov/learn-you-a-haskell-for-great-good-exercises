import System.IO
import System.Directory
import Data.List

todosFile :: String
todosFile = "todos.txt"

main = do
  content <- readFile todosFile
  let tasks = lines content
      numberedTasks = zipWith (\line n -> show n ++ ", " ++ line) tasks [0..]

  putStrLn "Here are you todos:"
  mapM_ putStrLn numberedTasks

  putStrLn "Enter the number of the todo item you want to delete?"
  toDeleteNum <- getLine

  let number = read toDeleteNum
      newTodos = unlines $ delete (tasks !! number) tasks

  putStrLn $ "You're going to delete: " ++ show number
  writeFile todosFile newTodos
