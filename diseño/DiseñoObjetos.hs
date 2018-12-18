
data Persona = Persona Int String
 -- extend Object -- algunas propiedades comunes de objetos
edad (Persona i _) = i
nombre (Persona _ nom) = nom

data Empleado = Empleado String Persona
edad2 (Empleado _ self) = edad self
nombre2 (Empleado _ self) = nombre self

main = let per = Persona 43 "pepe"
           empl = Empleado "Bancario" per in
           do putStrLn $ nombre2 empl
           
{- Buscar forma de transformar 
  nombre persona    -> persona#nombre 
  -}