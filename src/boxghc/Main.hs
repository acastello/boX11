{-# LANGUAGE OverloadedStrings #-}

import System.Process
import System.Environment
import System.Posix

import Control.Monad (when)

import qualified Data.ByteString.Lazy as BS
import Text.Parse.ByteString

exportLines = "foreign export ccall \"box_main\" main :: IO ()"

main = do
    let compiler = "ghc"
    args@(target:_) <- getArgs
    vers <- return . (filter (/= '\n')) =<< readProcess compiler ["--numeric-version"] "" 
    let rts = "/usr/lib/ghc-" ++ vers ++ "/rts/libHSrts-ghc" ++ vers ++ ".so"
    print [show args , vers, rts]
    print =<< readProcess "ls" [rts] ""
    return ()
    
fixFile :: FilePath -> IO ()
fixFile f = do
    sz <- return . fileSize =<< getFileStatus f
    print sz

    BS.appendFile f exportLines
    print =<< BS.readFile f
    setFileSize f sz
    return ()

