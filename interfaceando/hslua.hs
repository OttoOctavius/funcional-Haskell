import Foreign.Lua as Lua

-- | A value that can be read from the Lua stack.
class Peekable a where
    -- | Check if at index @n@ there is a convertible Lua value and
    --   if so return it.  Throws a @'LuaException'@ otherwise.
    peek :: StackIndex -> Lua a
  -- | A value that can be pushed to the Lua stack.
class Pushable a where
    -- | Pushes a value onto Lua stack, casting it into meaningfully
    --   nearest Lua type.
    push :: a -> Lua ()

main :: IO ()
main = Lua.run prog
  where
    prog :: Lua ()
    prog = do
      Lua.openlibs  -- load Lua libraries so we can use 'print'
      pushHaskellFunction (putStr)
      setglobal "putStr"
      Lua.callFunc "putStr" "yo"