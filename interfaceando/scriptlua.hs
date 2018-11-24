
import Foreign.Lua as Lua

main :: IO ()
main = Lua.run prog
  where
    prog :: Lua ()
    prog = do
      Lua.openlibs  -- load Lua libraries so we can use 'print'
      Lua.callFunc "print" "Hello, World!"