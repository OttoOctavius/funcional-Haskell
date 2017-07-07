-- Recordemos

-- foldr :: (a -> b -> b) -> [a] -> b -> [b]

-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- concatMap :: (a -> [b]) -> [a] -> [b]

-- all/any :: (a -> Bool) -> [a] -> Bool
-- and/or :: [Bool] -> Bool
-- sum/product :: Num a => [a] -> a

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
-- zipWith _ _ _ = []

-- zip :: [a] -> [b] -> [(a,b)]
-- zip = zipWith (,)


index :: [a] -> [(Int,a)]
index = zip [1..]


{-
Queremos ver que: sum (xs ++ ys) = sum (zipWith (+) xs ys)

Caso base (xs = []):

        sum ([] ++ ys) = sum (zipWith (+) [] ys)
    zipWith.2 => sum ([] ++ ys) = sum []
    (++).1 => sum ys = sum []
    
        ¿Qué pasó?
		Ver demostración al final
-}


type Matrix a = [[a]]
-- Invariante: Todas las listas internas tienen la misma longitud (repok)

repok :: Matrix a -> Bool
repok (xs:xss) = all (length xs ==) (map length xss)


id_3x3 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]::Matrix Int
ma_3x3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]::Matrix Int 


mGet :: Matrix a -> Int -> Int -> a
mGet m i j = (m !! i) !! j


mulScalar :: Int -> Matrix Int -> Matrix Int
mulScalar n x = map (map (n*)) x


mAdd :: Matrix Int -> Matrix Int -> Matrix Int
mAdd x y = zipWith (zipWith (+)) x y


mAll :: (a -> Bool) -> Matrix a -> Bool
mAll p x = and (map (all p) x)


{- DESAFÍO: Definir zipWith sin utilizar recurisón explícita, es decir, utilizando
   únicamente esquemas (i.e. foldr).
   ACLARACIÓN: No vale usar zip, ni ninguna función auxiliar definida por recursión.
   Sí es válido usar auxiliares si están definidas con esquemas. En particular
   definir zip con esquemas conlleva la misma dificultad que definir zipWith. -}



{- Una propiedad que sí vale:

Vemos que: length (zipWith f xs ys) = min (length xs) (length ys)

Caso base (xs = []):

        length (zipWith f [] ys) = min (length []) (length ys)
    zipWith.2 => length [] = min (length []) (length ys)
    length.1  => 0 = min 0 (length ys)
    Lema 1 (length ys >= 0) => True

    
Paso inductivo:
        
        length (zipWith f (x:xs) ys) = min (length (x:xs)) (length ys)
    length.2 => length (zipWith f (x:xs) ys) = min (1 + length xs) (length ys)
    
    Para demostrar esto necesitamos otra demostración por inducción.
    Esta vez en la estructura de ys.

    Caso base (ys = []):
                length (zipWith f (x:xs) []) = min (1 + length xs) (length [])
            zipWith.2 => length [] = min (1 + length xs) (length [])
            length.1  => 0 = min (1 + length xs) 0
            Lema 1    => 0 = 0

    Paso inductivo (ys = y:ys', asumiendo que vale para ys'):
                 length (zipWith f (x:xs) (y:ys')) = min (1 + length xs) (length (y:ys'))
            zipWith.1 => length (f x y : zipWith f xs ys') = min (1 + length xs) (length (y:ys'))
            length.2  => 1 + length (zipWith f xs ys') = min (1 + length xs) (1 + length ys')
            Lema 2 (min (1+n) (1+m) = 1 + min n m) =>
                1 + length (zipWith f xs ys') = 1 + min (length xs) (length ys')
            arit      => length (zipWith f xs ys') = min (length xs) (length ys')
            HI        => True
-}

