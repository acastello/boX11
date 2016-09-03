module TTree where

data TTree a b = TNode a b | TForest a [TTree a b]
    deriving Show

drawTTree ttree = unlines $ draw' ttree where
    draw' (TNode i x) = concat [show i, " -> ", show x] : []
    draw' (TForest i xs) = shift (show i ++ " +- ") " |- " (head xs)
    shift first other = zipWith (++) (first : repeat other)
instance Functor (TTree a) where
    fmap = fmapTTree

fmapTTree f (TForest i xs) =
    TForest i (fmap (fmapTTree f) xs)
fmapTTree f (TNode a b) = TNode a (f b)

instance Monoid a => Applicative (TTree a) where
    pure b = TNode (mempty) b
    (<*>) = seqTTree

seqTTree (TNode a f) ttree = fmap f ttree
seqTTree ftree (TNode i e) = fmap ($ e) ftree
seqTTree ftree (TForest i xs) = TForest i (fmap (seqTTree ftree) xs)


instance Show (IO a) where
    show _ = "()"
type Binds = TTree Int (IO ())

u = TNode "u0" 1
v = TForest "v0" [TNode "v00" 2, TNode "v01" 3]
w = TForest "w0" [TForest "w00" [TNode "000" 4, TNode "001" 5], TNode "w01" 6, TForest "w02" [TForest "w020" [TNode "w0200" 7]]]
