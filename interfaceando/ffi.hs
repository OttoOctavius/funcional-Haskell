{-# LANGUAGE ForeignFunctionInterface #-}
-- no puede faltar esa linea

foreign import ccall "exp" c_exp :: Double -> Double

triple :: Int -> Int
triple x = 3*x

foreign export ccall triple :: Int -> Int