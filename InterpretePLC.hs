
data Instruccion =   LD Var (Instruccion) | LDN Var (Instruccion) |
                     OR Var (Instruccion) | NOR Var (Instruccion) |
                     AND Var (Instruccion)| NAND Var (Instruccion) |
                     SET Var | RST Var | 

data Var = IN String | OUT String 

progbasico = LD (IN "0") (SET (OUT "1"))

eval :: Instruccion -> [Var]
eval (LD Var inst) = Var : (eval inst) 