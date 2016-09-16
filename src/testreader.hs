{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class

f1 :: Reader String Int
f1 = do
    content <- ask
    return (length content)

f2 :: Reader String Int
f2 = local ("Prefix " ++) f1


f3 :: ReaderT String IO ()
f3 = do
    content <- ask
    liftIO $ putStrLn "sad"
    liftIO $ putStrLn content

f4 :: ReaderT String [] String
f4 = do
    c <- ask
    return c

f5 :: ReaderT String [] String
f5 = do
    return "aasasdasdasd"

newtype N a = N (ReaderT String IO a)
    deriving (Functor, Applicative, Monad, MonadReader String)

runN :: N a -> String -> IO a
runN (N a) str = runReaderT a str

n1 :: N (String, String)
n1 = do
    str <- ask
    return (str ++ "1", str ++ "2")
