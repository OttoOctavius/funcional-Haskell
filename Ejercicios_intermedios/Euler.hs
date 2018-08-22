
sumaDeMultiplosDe3o5Hasta1000  = sum (multiplosDe 3 5)
    where multiplosDe n m = [x | x <- [1..1000], x  `mod` n==0 || x `mod` m==0 ]
