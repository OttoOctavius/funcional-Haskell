import Data.Array

main = let xs = array (0,10) [(i, i * i) | i <- [0..10]]
        in do
                putStr $ "Limites son: " ++ show (bounds xs)
                
