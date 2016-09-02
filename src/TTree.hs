module TTree where

data TTree a b = TNode a b | TForest a [TTree a b]
    deriving Show

instance Functor (TTree a) where
    fmap = fmapTTree

fmapTTree f (TForest i xs) =
    TForest i (fmap (fmapTTree f) xs)
fmapTTree f (TNode a b) = TNode a (f b)

instance Monoid a => Applicative (TTree a) where
    pure b = TNode (mempty) b
    (<*>) = seqTTree

seqTTree ftree (TNode i e) = fmap ($ e) ftree
seqTTree ftree (TForest i xs) = TForest i (fmap (seqTTree ftree) xs)

type Binds = TTree Int (IO ())

