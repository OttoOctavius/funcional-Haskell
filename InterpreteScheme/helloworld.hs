module Main where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import System.Environment
    
    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    readExpr :: String -> String
    readExpr input = case parse symbol "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value"

    main :: IO ()
    main = do 
            (expr:_) <- getArgs
            putStrLn (readExpr expr)
    
    
    main0 :: IO ()
    main0 = do
        args <- getArgs
        let x = read $ args !! 0
            y = read $ args !! 1 
        putStrLn ("Hello, " ++ show (x+y))