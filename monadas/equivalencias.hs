import Control.Monad

main = do
          x <- return $ 3
          x
          --bind (Just 3) (+1) 