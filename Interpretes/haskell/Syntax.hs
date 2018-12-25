module Syntax where

import Eval
import Parser
import Pretty

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Parsec
 
process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result




parseExpr s = parse (contents expr) "<stdin>" s

prefixOp s f = Ex.Prefix (reservedOp s >> return f)


-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      prefixOp "succ" Succ
    , prefixOp "pred" Pred
    , prefixOp "iszero" IsZero
    ]
  ]


-- if/then/else
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- Constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0"   >> return Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      true
  <|> false
  <|> zero
  <|> ifthen
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' x = case x of
    IsZero Zero               -> Just Tr
    IsZero (Succ t) | isNum t -> Just Fl
    IsZero t                  -> IsZero <$> (eval' t)
    Succ t                    -> Succ <$> (eval' t)
    Pred Zero                 -> Just Zero
    Pred (Succ t) | isNum t   -> Just t
    Pred t                    -> Pred <$> (eval' t)
    If Tr  c _                -> Just c
    If Fl _ a                 -> Just a
    If t c a                  -> (\t' -> If t' c a) <$> eval' t
    _                         -> Nothing

nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"

