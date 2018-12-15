import Data.Array --http://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html
import Control.Monad --Para usar el forM_
--import Control.Monad.LoopWhile
--import Data.Vector ((!), generate)
import Data.Dynamic
--Nuevos: http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html
--basado en https://en.wikipedia.org/wiki/Finger_tree

--Deben ser del tipo "Typeable"
defaul :: Integer 
defaul = 0
idayvuelta = fromDyn (toDyn (3::Integer)) defaul
--fromDynamic :: Typeable a => Dynamic -> Maybe a
mostrarDynamic = putStrLn (show idayvuelta)



{-
buyable n = r!n
  where
    r = generate (n+1) f
    f i = i == 0 || i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)
-}

--Este tipo de definicion de array permite recursion sobre "xs"
accesos = let xs = array (0,10) [(i, i * i) | i <- [0..10]]
     in do
        putStrLn $ "Limites son: " ++ show (bounds xs)
        putStrLn $ "Indices son: " ++ show (indices xs)
        putStrLn $ "Elementos son: " ++ show (elems xs)
        putStrLn $ "Primer elemento: " ++ show ( assocs xs !!0)
        forM_ [0..10] (\x -> putStrLn $ show (xs!x))

        {-
incremUpdates = let lista = listArray (0,2) [10,20,30]
        in loop $ do return lista
                while (lista!0 < 0)
        -}

main = 
