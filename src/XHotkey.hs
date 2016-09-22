module XHotkey 
  ( module XHotkey.Types
  ) where  

import XHotkey.Types

import Data.Word

data D = A Int | B Word16 | C Word32
    deriving (Show)
