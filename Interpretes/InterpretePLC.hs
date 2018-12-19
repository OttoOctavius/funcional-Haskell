import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map as Map

{-Colocar todas las instrucciones en un dato algebraico.
  Las variables son las que pueden estar negadas o no. Ademas establece el nombre y tipo
  de la compuerta.
  Al final, instrucciones para almacenar resultados.  -}


data Instruccion =   LD Var (Instruccion) |
                     OR Var (Instruccion) |
                     AND Var (Instruccion)|
                     SET Var | RST Var | HOLD Var

data Var_t t = IN t | OUT t | NIN t | NOUT t
newtype Var = Var_t String

iniciar_variables = Map.empty
esta_variable dicc = \var. member var dicc
setear_variable nombre bool = insert nombre bool 
--adjust f->f key dicc    si se quiere con f->Maybe f, usar update 

progbasico = LD (IN "0") (SET (OUT "1"))

{-
eval :: Instruccion -> Var
eval (LD var inst) = var : (eval inst)
eval (AND var inst)= var : (eval inst)
eval (OR var inst) = var : (eval inst)
    where buscar_var var = do env <- ask
                              case Map.lookup n env of
                                Nothing -> throwError "Falta esa variable"
                                Just val -> return val
-}
guardar (SET v) b = do env <- ask
                       if b then Map.insert v b env
                            else return env
guardar (RST v) b = guardar (SET v) (!b)
guardar (HOLD v) b = do env  <- ask
                        env' <- guardar (SET v) b env
                        guardar (RST v) b env'

--leer :: Instruccion -> Maybe Bool
leer (LD var inst) = do env <- ask
                        return $ Map.lookup var env
leer (AND var inst)= do env <- ask
                        return $ Map.lookup var env
leer (OR var inst) = do env <- ask
                        return $ Map.lookup var env

                        --