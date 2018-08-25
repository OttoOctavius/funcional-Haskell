import Data.List  -- (\\) is set-difference for unordered lists

sumaDeMultiplosDe3o5Hasta1000  = sum (multiplosDe 3 5)
    where multiplosDe n m = [x | x <- [1..1000], x  `mod` n==0 || x `mod` m==0 ]

primesTo m = ps 
             where
             ps = map head $ takeWhile (not.null) 
                           $ scanl (\\) [2..m] [[p, p+p..m] | p <- ps]
{-Numeros primos, para el numero 600851475143 -}
sumadeprimosDe600851475143 = max [ x | x <- primesTo 600851475143]

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
sumaEvenfibonassi = sum [x | x <- fibs, x < 4000000 && even x ]

isPalindrome''' = Control.Monad.liftM2 (==) id reverse
palindromosDe3Cifras = [x*y| x <- [100..999], y <- [100..999] , isPalindrome''' $show(x*y)]
mayorPalindromo = max palindromosDe3Cifras
