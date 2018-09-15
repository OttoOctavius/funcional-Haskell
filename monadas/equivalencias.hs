import Control.Monad
import Control.Concurrent
import Data.IORef
--import Control.Either
foreign import ccall "exp" c_exp :: Double -> Double
--foreign import swap "swap" c_swap:: Double -> Double

{-
newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()

data STRef s a
newSTRef    :: a -> ST s (STRef s a)
readSTRef   :: STRef s a -> ST s a
writeSTRef  :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
-}
maybePrint :: IORef Bool -> IORef Bool -> IO ()
maybePrint myRef yourRef = do
  writeIORef myRef True
  yourVal <- readIORef yourRef
  unless yourVal $ putStrLn "critical section"

main :: IO ()
main = do
  print (c_exp 0)
  r1 <- newIORef False
  r2 <- newIORef False
  forkIO $ maybePrint r1 r2
  forkIO $ maybePrint r2 r1
  threadDelay 1000000
