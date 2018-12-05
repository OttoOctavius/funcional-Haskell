import Data.Array --http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html
import Control.Monad --Para usar el forM_
import Control.Monad.LoopWhile

--Este tipo de definicion de array permite recursion sobre "xs"
accesos = let xs = array (0,10) [(i, i * i) | i <- [0..10]]
     in do
        putStrLn $ "Limites son: " ++ show (bounds xs)
        putStrLn $ "Indices son: " ++ show (indices xs)
        putStrLn $ "Elementos son: " ++ show (elems xs)
        putStrLn $ "Primer elemento: " ++ show ( assocs xs !!0)
        forM_ [0..10] (\x -> putStrLn $ show (xs!x))

incremUpdates = let lista = listArray (0,2) [10,20,30]
        in loop $ do return lista
                while (lista!0 < 0)

main = incremUpdates --accesos