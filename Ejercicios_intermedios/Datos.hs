import Data.Array --http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html

main = let xs = array (0,10) [(i, i * i) | i <- [0..10]]
        in do
                putStrLn $ "Limites son: " ++ show (bounds xs)
                putStrLn $ "Indices son: " ++ show (indices xs)
                putStrLn $ "Elementos son: " ++ show (elems xs) 
