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

type Env = Map.Map Name Value



type Eval2 a = ErrorT String Identity a
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

runEval3 :: Env ->  Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runErrorT (runReaderT ev env)

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                        Nothing -> throwError "type error"
                        Just val -> return val
eval3 (Plus e1 e2) =      do e1' <- eval3 e1 
                             e2' <- eval3 e2
                             case (e1',e2') of
                                 (IntVal i1,IntVal i2) -> return $ IntVal (i1 + i2)
                                 _ -> throwError "type error"
eval3  (Abs n e) = do env <- ask
                      return $ FunVal env n e
eval3  (App e1 e2) = do val1 <- eval3 e1
                        val2 <- eval3 e2
                        case val1 of
                             FunVal env0 n body -> local (const(Map.insert n val2 env0)) (eval3 body)
                             _ -> throwError "type error"

--runEval3 Map.empty (eval3 exampleExp)
{- Que hace local
The local function is used for modifying the environment for the recursive call. Local has the type (r ? r) ? m a ? m a, that is we need to pass in a function which maps the current environment to the one to be used in the nested computation, which is the second argument. In our case, the nested environment does not depend on the current environment, so we simply pass in a constant function using const.
-}

--Loogin, puede interponerse en con ErrorT y mostrar en output.

type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a,[String]),Integer) 
runEval5 env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                        Nothing -> throwError "Variable no aparece"
                        Just val -> return val
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1',e2') of
                             (IntVal i1,IntVal i2) -> return $ IntVal (i1 + i2)
                             _ -> throwError "type error"
eval5 (Abs n e) = do tick 
                     env <- ask 
                     return $ FunVal env n e
eval5 (App e1 e2) = do tick 
                       val1 <- eval5 e1 
                       val2 <- eval5 e2
                       case val1 of
                            FunVal env0 n body -> local (const (Map.insert n val2 env0))
                                          (eval5 body)
                            _ -> throwError "type error in application"










