import System.Environment  
import System.IO  
import System.Directory  
import Data.String

data Tokens = Import String

tokenizar contenido
    | contains (words contenido)  "import" = Import contenido

main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ (lines contents) !!0 ++ " lines!"  --show (length (lines contents))
              else do putStrLn "The file doesn't exist!"  
