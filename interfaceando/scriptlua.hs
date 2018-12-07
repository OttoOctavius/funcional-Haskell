
import Foreign.Lua as Lua
--import OOHaskell
--Wall para los warnings

main :: IO ()
main = Lua.run prog
  where
    prog :: Lua ()
    prog = do
      Lua.openlibs  -- load Lua libraries so we can use 'print'
      Lua.callFunc "print" "Hello, World!"