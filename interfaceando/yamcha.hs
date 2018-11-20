{-# LANGUAGE Arrows #-}
module ArrowFun where
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))
import Control.Monad

newtype SimpleFunc a b = SimpleFunc {
    runF :: (a -> b)
}

instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
                  where mapFst g (a,b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
                  where mapSnd g (a,b) = (a, g b)
instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
    id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))
--Unsplit is an arrow that takes a pair of values and combines them to return a single value:
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry
liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x*3 + 1)
--We can combine these together using liftA2:
--LiftA2 makes a new arrow that combines the output from two arrows using a binary operation. It works by splitting a value and operating on both halfs and then combining the result:
h :: SimpleFunc Int Int
h = liftA2 (+) f g

hOutput :: Int
hOutput = runF h 8

--main :: IO ()
main = do
    putStrLn $ show (hOutput)
    putStrLn " chau"
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  