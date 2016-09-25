module XHotkey 
  ( module XHotkey.Types
  , module XHotkey.Core
  ) where  

import XHotkey.Types
import XHotkey.Core
import MapTree

import Data.Word

import Control.Monad.State

data D = A Int | B Word16 | C Word32
    deriving (Show)

binds :: Bindings
binds = mapKeys read $ fromList
    [ "C-slash" .< 
        [ "C-slash" .> liftIO ( putStrLn "C-/")
        , "M-c\\" .> (return ()) ]
    , "1" .> put $ XControl mempty True 
    , "A" .< ["a" .< ["a" .> return ()]]]
    
