
data Persona = Persona Int String

edad (Persona i _) = i

nombre (Persona _ nom) = nom
 -- extend Object -- algunas propiedades comunes de objetos

main = let per = Persona 3 "pepe" in
           do putStrLn $ nombre per
           
{- Buscar forma de transformar 
  nombre persona    -> persona#nombre 
  -}