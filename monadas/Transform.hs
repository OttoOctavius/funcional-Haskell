module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer -- values 
           | FunVal Env Name Exp 
           deriving (Show)

-- mapping from names to values
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2 
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                           FunVal env0 n body -> eval0 (Map.insert n val2 env0) body

type Eval2 a = ErrorT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1 
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                 FunVal env0 n body -> eval2a (Map.insert n val2 env0) body


eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1 
                             e2' <- eval2b env e2
                             case (e1',e2') of
                               (IntVal i1,IntVal i2) -> return $ IntVal (i1 + i2)
                               _ -> throwError "type error"

eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                                 FunVal env0 n body -> eval2a (Map.insert n val2 env0) body
                                 _ -> throwError "type error"


-- runEval2 (eval2a Map.empty exampleExp) -- igual a: runEval2  eval0 Map.empty (Lit 0)