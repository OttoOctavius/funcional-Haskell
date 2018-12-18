--Esquemas de recursión en tipos complejos

data Tree a = Nil | Bin a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Bin e lc rc) = Bin (f e) (mapTree f lc) (mapTree f rc)


foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f cb Nil = cb
foldTree f cb (Bin e lc rc) = f e (foldTree f cb lc) (foldTree f cb rc)


mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f = foldTree (\e lc rc -> Bin (f e) lc rc) Nil


countTree = foldTree (\e lr rr -> 1 + lr + rr) 0

heightTree = foldTree (\e lr rr -> 1 + max lr rr) 0


inorder :: Tree a -> [a]
inorder = foldTree (\e lr rr -> lr ++ (e:rr)) []


-- Consider a search tree (one in which each root of a subtree is the less than the
-- minimum of the left child and greater that the maximum of the right child)

search :: Ord a => a -> Tree a -> Bool
search e = foldTree (\e' lr rr -> e == e' || if e < e' then lr else rr) False



data Poli = Cte Int | Var | Add Poli Poli | Mul Poli Poli

foldp :: (Int -> a) -> a -> (a -> a -> a) -> (a -> a -> a) -> Poli -> a
foldp c v a m (Cte n)   = c n
foldp c v a m (Var)     = v
foldp c v a m (Add p q) = a (foldp c v a m p) (foldp c v a m q)
foldp c v a m (Mul p q) = m (foldp c v a m p) (foldp c v a m q)


eval :: Poli -> Int -> Int
eval p n = foldp id n (+) (*) p



data PrefixTree = Nil | Node a (PrefixTree a) (PrefixTree a) (PrefixTree a)

foldt :: (a -> b -> b -> b) -> b -> PrefixTree a -> b
foldt f b Nil = b
foldt f b (Node a l c r) = f a (foldt f b l) (foldt f b c) (foldt f b r)

flatten :: Ord a => PrefixTree a -> [[a]]
flatten = foldt (\e l c r -> lr ++ (map (e:) c) ++ filter (not.null) r) [[]]


recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f c []     = c
recr f c (x:xs) = f x xs (recr f c xs)


insert :: Ord a => a -> [a] -> [a]
insert e = recr (\x xs xs' -> if x > e then (e:x:xs) else (x:xs'))


{- DESAFÍO: Encontrar la longitud del camino más largo en un árbol (el camino más largo
   es una secuencia de nodos conectados que comienza en una hoja y termina en una hoja).
   Note que el camino más largo no necesariamente pasa por la raíz. No puede usar recursión
   de forma explícita en la función ni en ninguna función auxiliar salvo en la función
   foldTree que generaliza la recursión en árboles.

   longest :: Tree a -> Int

 -}
