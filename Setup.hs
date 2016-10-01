import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Run
import Distribution.Verbosity

import Control.Monad (when)

main = defaultMainWithHooks simpleUserHooks 
    { postBuild = \args flags desc info -> do
        let LocalBuildInfo { buildDir = dist } = info
            BuildFlags { buildVerbosity = Flag v } = flags
            cmd = "winegcc -shared -fPIC src/boX11.c -o " ++ dist ++ "/libboX11.dll.so" ++ (if v >= verbose then " -v" else "") ++ "; mv " ++ dist ++ "/libboX11.dll.so " ++ dist ++ "/libboX11.so"
        when (v /= silent) $ putStrLn cmd
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd]

    , postCopy = \args flags desc info -> do
        let LocalBuildInfo 
                { buildDir = dist
                , installDirTemplates = InstallDirs { libdir = d, libsubdir = sd }} = info
            CopyFlags { copyVerbosity = Flag v } = flags
            dir = fromPathTemplate d
            subdir = fromPathTemplate sd
            targetdir = dir ++ "/" ++ subdir
            cmd = "mv " ++ dist ++ "/libboX11.so " ++ targetdir ++ "/"

        when (v /= silent) $ putStrLn cmd
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd]        
    }

