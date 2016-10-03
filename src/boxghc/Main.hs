import System.Process
import System.Environment
import System.Directory
import System.Posix
import System.FilePath
import System.Posix.Temp
import System.Exit

import Control.Monad (when)
import Control.Exception
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, catMaybes)

exportLines = "-- lines added by boxghc, delete these if you can read them after compiling\nforeign export ccall \"box_main\" main :: IO ()"

main = do
    let compiler = "ghc"
    args <- getArgs
    vers <- return . (filter (/= '\n')) =<< readProcess compiler ["--numeric-version"] "" 
    let target = if args == [] then error "no target" else head args
        rts = "/usr/lib/ghc-" ++ vers ++ "/rts/libHSrts-ghc" ++ vers ++ ".so"
        verb = getVerbosity args
    fe <- fileExist rts
    when (not fe) $ error $ "couldn't find runtime library " ++ rts
    tmpdir <- mkdtemp "box"
    let target' = replaceDirectory target $ tmpdir
    finally 
        (do
            copyFile target target'
            appendFile target' exportLines 
            let cmd = (target' : tail args) ++ ["-dynamic", "-shared", "-threaded", "-fPIC", "-lboX11", "-outputdir", tmpdir, rts]
            when (verb > 0) $ 
                putStrLn $ foldr1 (\a b -> a ++ ' ':b) (compiler:cmd)
                
            waitForProcess =<< runProcess compiler cmd Nothing Nothing Nothing Nothing Nothing
        )
        (do
            when (not $ elem "-keep-tmp-files" args) $ removeDirectoryRecursive tmpdir )
    return ()

getVerbosity :: [String] -> Integer
getVerbosity xs = last $ catMaybes $ Just 1:(v <$> xs)
    where v str = if "-v" `isPrefixOf` str then readMaybe $ drop 2 str else Nothing
