
data Instruccion a = Load a (Instruccion a) | Paralel a (Instruccion a) | Secuencial a (Instruccion a) | Set a
data Var = Pos String |  Negado String 

progbasico = Load (Pos "IN1") (Set (Pos "OUT1"))
