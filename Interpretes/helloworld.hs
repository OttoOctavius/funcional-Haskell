module Main where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import System.Environment
    import Control.Monad

    data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool

    instance Show LispVal where show = showVal
    
    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    parseString :: Parser LispVal
    parseString = do
                    char '"'
                    x <- many (noneOf "\"")
                    char '"'
                    return $ String x
    parseAtom :: Parser LispVal
    parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom
    parseNumber :: Parser LispVal
    parseNumber = liftM (Number . read) $ many1 digit

    spaces :: Parser ()
    spaces = skipMany1 space
    
    parseList :: Parser LispVal
    parseList = liftM List $ sepBy parseExpr spaces

    parseDottedList :: Parser LispVal
    parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail
    parseQuoted :: Parser LispVal
    parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]
        
    parseExpr :: Parser LispVal
    parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do  char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x

    readExpr :: String -> LispVal
    readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val

    showVal :: LispVal -> String
    showVal (String contents) = "\"" ++ contents ++ "\""
    showVal (Atom name) = name
    showVal (Number contents) = show contents
    showVal (Bool True) = "#t"
    showVal (Bool False) = "#f"
    showVal (List contents) = "(" ++ unwordsList contents ++ ")"
    showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

    unwordsList :: [LispVal] -> String
    unwordsList = unwords . map showVal

    eval :: LispVal -> LispVal
    eval val@(String _) = val
    eval val@(Number _) = val
    eval val@(Bool _) = val
    eval (List [Atom "quote", val]) = val
            
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