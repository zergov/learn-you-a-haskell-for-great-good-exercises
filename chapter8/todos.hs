import System.IO
import System.Directory
import System.Environment
import Data.List

todosFile :: String
todosFile = "todos.txt"

main = do
  (command:argList) <- getArgs
  dispatch command argList

dispatch :: String -> [String] -> IO()
dispatch "add" = add
dispatch "bump" = bump
dispatch "list" = list
dispatch "ls" = list
dispatch "remove" = remove
dispatch "rm" = remove

add :: [String] -> IO()
add [todo] = do
  appendFile todosFile (todo ++ "\n")
  putStrLn $ "added to " ++ todosFile

list :: [String] -> IO()
list _ = do
  content <- readFile todosFile
  let tasks = zipWith (\n line -> show n ++ ", " ++ line) [0..] (lines content)
  mapM_ putStrLn tasks

remove :: [String] -> IO()
remove _ = do
  content <- readFile todosFile
  let tasks = lines content
      numberedTasks = zipWith (\line n -> show n ++ ", " ++ line) tasks [0..]

  putStrLn "Here are you todos:"
  mapM_ putStrLn numberedTasks

  putStrLn "Enter the number of the todo item you want to delete?"
  toDeleteNum <- getLine

  let number = read toDeleteNum
      newTodos = unlines $ delete (tasks !! number) tasks

  writeFile todosFile newTodos
  putStrLn $ "task: " ++ (show number) ++ " deleted."

bump :: [String] -> IO()
bump [todo] = do
  content <- readFile todosFile
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines $ todo : lines content
  hClose tempHandle
  removeFile todosFile
  renameFile tempName todosFile
  putStrLn $ "Task " ++ todo ++ " bumped to the top!"
