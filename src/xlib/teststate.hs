import Data.IORef
import GHC.IO (unsafePerformIO)

ref :: IORef Integer
ref = unsafePerformIO (newIORef 1)

main = do
    readIORef ref >>= print
    modifyIORef ref (+1) 
    readIORef ref >>= print
    modifyIORef ref (+1) 
    readIORef ref >>= print
    modifyIORef ref (+1) 
    readIORef ref >>= print
