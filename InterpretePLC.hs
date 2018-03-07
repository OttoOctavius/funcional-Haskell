
data Instruccion =   LD Var (Instruccion) |
                     OR Var (Instruccion) |
                     AND Var (Instruccion)|
                     SET Var | RST Var | HOLD Var

data Var_t t = IN t | OUT t | NIN t | NOUT t
newtype Var = Var_t String


progbasico = LD (IN "0") (SET (OUT "1"))

eval :: Instruccion -> [Var]
eval (LD Var inst) = Var : (eval inst) 