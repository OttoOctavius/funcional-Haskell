{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Data.Monoid
import Foreign.Lua as Lua

main :: IO ()
main = Lua.run $ do
  openlibs
  registerHaskellFunction "concat" concat'
  registerHaskellFunction "pow" pow
  registerHaskellFunction "helloWorld" helloWorld
  loadfile "interfaceando/haskellfun.lua"
  call 0 0
  
--No se llama al codigo que falla

concat' :: B.ByteString -> B.ByteString -> Lua B.ByteString
concat' s1 s2 = return $ s1 <> s2

pow :: Lua.Number -> Lua.Number -> Lua Lua.Number
pow d1 d2 = return $ d1 ** d2

helloWorld :: Lua B.ByteString
helloWorld = return "Hello, World!"