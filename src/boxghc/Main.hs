{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Environment
import System.Posix
import System.Exit

import Control.Monad (when)
import Control.Exception

import qualified Data.ByteString as BS

exportLines :: BS.ByteString
exportLines = "-- lines added by boxghc, delete these if you can read them after compiling\nforeign export ccall \"box_main\" main :: IO ()"

main = do
    let compiler = "ghc"
    args <- getArgs
    let target = if args == [] then "Main.hs" else head args
    vers <- return . (filter (/= '\n')) =<< readProcess compiler ["--numeric-version"] "" 
    let rts = "/usr/lib/ghc-" ++ vers ++ "/rts/libHSrts-ghc" ++ vers ++ ".so"
    fe <- fileExist rts
    when (not fe) $ error $ "couldn't find runtime library " ++ rts
    print [show args , vers, rts]
    sz <- return . fileSize =<< getFileStatus target
    finally 
        (do
            BS.appendFile target exportLines 
            waitForProcess =<< runProcess compiler (args ++ ["-dynamic", "-shared", "-threaded", "-fPIC", "-lboX11", rts]) Nothing Nothing Nothing Nothing Nothing
        )
        (do
            setFileSize target sz )
    return ()

foreign export ccall "box_main" main :: IO ()
