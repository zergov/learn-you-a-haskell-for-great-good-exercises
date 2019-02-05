import System.IO

main = do
  contents <- readFile "girlfriend.txt"
  putStr contents

-- main = do
  -- withFile "girlfriend.txt" ReadMode (\handler -> do
    -- contents <- hGetContents handler
    -- putStr contents
    -- )
