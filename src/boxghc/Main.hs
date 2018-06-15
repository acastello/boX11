import System.Process
import System.Environment
import System.Directory
import System.Posix hiding (createDirectory)
import System.Posix.Files
import System.Exit
import System.FilePath
import System.IO
-- import System.Posix.Directory

import Control.Monad (when)
import Control.Exception
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, catMaybes)

exportLines = unlines
  [ "-- lines added by boxghc, delete these if you can read them after compiling"
  , "foreign export ccall \"box_main\" main :: IO ()"
  ]

launch soname = unlines
  [ "#!/bin/sh"
  , ""
  , "appname=\"$(dirname $0)/" ++ soname ++ "\""
  , "cabal exec -- boxlaunch.exe \"$appname\" \"$@\""
  ]

main = do
    let compiler = "ghc"
    args <- getArgs
    libdir <- filter (/= '\n') <$> readProcess compiler ["--print-libdir"] "" 
    vers <- filter (/= '\n') <$> readProcess compiler ["--numeric-version"] "" 
    let target = if args == [] then error "no target" else head args
        rts = libdir ++ "/rts/libHSrts_thr-ghc" ++ vers ++ ".so"
        verb = getVerbosity args
    fe <- fileExist rts
    when (not fe) $ error $ "couldn't find runtime library " ++ rts
    -- tmpdir <- mkdtemp "box"
    let tmpdir = ".boxghc"
    createDirectoryIfMissing False tmpdir
    let target' = replaceDirectory target $ tmpdir
        targetso = takeDirectory target </> takeBaseName target <.> "so"
        targetsh = takeDirectory target </> takeBaseName target
    finally 
        (do
            copyFile target target'
            appendFile target' exportLines 
            let cmd = (target' : tail args) ++ 
                  [ "-o", targetso, "-dynamic", "-shared", "-threaded", "-fPIC" 
                  , "-lboX11" , "-XOverloadedStrings", "-tmpdir", tmpdir
                  , "-outputdir", tmpdir, rts]
            when (verb > 0) $ 
                putStrLn $ foldr1 (\a b -> a ++ ' ':b) (compiler:cmd)
                
            e <- waitForProcess =<< runProcess compiler cmd Nothing Nothing Nothing Nothing Nothing
            when (e == ExitSuccess) $ do
                writeFile targetsh (launch targetso)
                setFileMode targetsh (stdFileMode `unionFileModes` ownerExecuteMode)
        )
        (do
            when (not $ elem "-keep-tmp-files" args) $ removeDirectoryRecursive tmpdir )
    return ()

getVerbosity :: [String] -> Integer
getVerbosity xs = last $ catMaybes $ Just 1:(v <$> xs)
    where v str = if "-v" `isPrefixOf` str then readMaybe $ drop 2 str else Nothing
