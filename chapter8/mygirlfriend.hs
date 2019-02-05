import System.IO
import Data.Char

girlfriend :: String
girlfriend = "Heloise Plante"

main =
  do
    writeFile "girlfriend.txt" girlfriend
