{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Environment
import System.Posix

import Control.Monad (when)
import Control.Exception

import qualified Data.ByteString as BS

exportLines :: BS.ByteString
exportLines = "\n \
    \ foreign export ccall \"box_main\" main :: IO ()"

main = do
    let compiler = "ghc"
    args <- getArgs
    let target = if args == [] then "Main.hs" else head args
    vers <- return . (filter (/= '\n')) =<< readProcess compiler ["--numeric-version"] "" 
    let rts = "/usr/lib/ghc-" ++ vers ++ "/rts/libHSrts-ghc" ++ vers ++ ".so"
    print [show args , vers, rts]
    print =<< readProcess "ls" [rts] ""
    return ()
    
fixFile :: FilePath -> IO ()
fixFile f = do
    sz <- return . fileSize =<< getFileStatus f
    BS.appendFile f exportLines
    setFileSize f sz

