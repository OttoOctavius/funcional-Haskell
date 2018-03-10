
data Instruccion =   LD Var (Instruccion) |
                     OR Var (Instruccion) |
                     AND Var (Instruccion)|
                     SET Var | RST Var | HOLD Var

data Var_t t = IN t | OUT t | NIN t | NOUT t
newtype Var = Var_t String


progbasico = LD (IN "0") (SET (OUT "1"))

eval :: Instruccion -> Var
eval (LD var inst) = var : (eval inst)
eval (AND var inst)= var : (eval inst)
eval (OR var inst) = var : (eval inst)
    where buscar_var var = do env <- ask
                              case Map.lookup n env of
                                Nothing -> throwError "Falta esa variable"
                                Just val -> return val