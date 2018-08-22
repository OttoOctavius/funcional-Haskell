import Data.List (\\)  -- (\\) is set-difference for unordered lists

sumaDeMultiplosDe3o5Hasta1000  = sum (multiplosDe 3 5)
    where multiplosDe n m = [x | x <- [1..1000], x  `mod` n==0 || x `mod` m==0 ]


{-Numeros primos, para el numero 600851475143 -}
sumadeprimosDe600851475143 = sum [ x | x <- primeTo 600851475143]

primesTo m = ps 
             where
             ps = map head $ takeWhile (not.null) 
                           $ scanl (\\) [2..m] [[p, p+p..m] | p <- ps]