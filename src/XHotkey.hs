module XHotkey 
  ( module XHotkey.Types
  , module XHotkey.Core
  ) where  

import XHotkey.Types
import XHotkey.Core

import Data.Word

data D = A Int | B Word16 | C Word32
    deriving (Show)
