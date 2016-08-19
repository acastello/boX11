module Export where

import Main
foreign export ccall "run" main :: IO ()
run = main

