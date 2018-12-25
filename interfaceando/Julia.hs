{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Julia where

import Control.Monad
import Foreign.C as C
import Foreign.C.String
--C.include "julia.h"

foreign import ccall "jl_init" jl_init :: IO ()
foreign import ccall "jl_eval_string($1)" jl_eval_string :: CString -> IO ()
foreign import ccall "jl_atexit_hook(0)" jl_atexit_hook :: IO ()
foreign import ccall "jl_errorf($1)" jl_errorf :: CString -> IO ()
    
evalJulia :: String -> IO ()
evalJulia = jl_eval_string <=< newCString
    
juliaInit = jl_init
exitJulia = jl_atexit_hook

main :: IO ()
main = do
  juliaInit
  evalJulia "print(sqrt(2.0))"
  exitJulia