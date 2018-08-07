import Data.Maybe

data Contador = Z | S Contador

pred3 x = do n <- pred x    -- pred3 x = pred x >>= \n ->
             m <- pred n    -- pred n >>= \m ->
             o <- pred m    -- pred m >>= \o ->
             return o       -- return o
  where pred Z = Nothing    --  where pred Z     = Nothing
        pred (S x) = Just x --        pred (S x) = Just x

papa xs = do
	x <- xs
	y <- map (+x) [4 5 6]
	return y

main = do y <- return "chau" >>= (\x-> putStrLn x) >> return 3
          return y
-- devuelve 3, e imprimio en pantalla

--isJust $ pred3 5
