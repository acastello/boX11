module TTree where

data TTree a b = TNode a b | TForest a [TTree a b]
    deriving Show

type Binds = TTree Int (IO ())

instance Show Binds where
    show _ = ""
