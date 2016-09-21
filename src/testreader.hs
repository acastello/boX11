{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class

import Data.Char (toLower)

import qualified Text.Read as T

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

newtype N a = N (ReaderT String (StateT Double IO) a)
    deriving (Functor, Applicative, Monad, MonadReader String, MonadState Double, MonadIO)

instance (Read a) => Read (N a) where
    readPrec =  do
                    m <- T.step T.readPrec
                    T.Symbol "-" <- T.lexP
                    return (return m)

runN :: N a -> String -> Double -> IO (a, Double)
runN (N a) str b = runStateT (runReaderT a str) b

n1 :: N (String, String)
n1 = do
    str <- ask
    n2 >>= liftIO . print
    get >>= put . (+1)
    return (str ++ "1", str ++ "2")

n2 :: N Int
n2 = do
    str <- ask
    modify (2*)
    return (length str)

data D = D {d1 :: String, d2 :: Integer }
    deriving Show

s1 :: StateT D IO String
s1 = do
    s <- get
    put $ s {d1 = "dsa"}
    return (d1 s)

newtype T = T String
    deriving Show

instance Read T where
    readPrec = do
                s <- lit "AhaM"
                return (T s)
            where
                state pre = do 
                    s <- T.lexP
                    return (show s)
                lit = mapM (\c -> T.get >>= \c' -> 
                    if toLower c' == (toLower c) then return c' else fail "") 
                    :: String -> T.ReadPrec String
