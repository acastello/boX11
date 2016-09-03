module TTree where

data TTree a b = TNode a b | TForest a [TTree a b]
    deriving (Show, Eq)

drawTTree :: (Show a, Show b) => TTree a b -> String
drawTTree ttree = unlines' $ draw' ttree where
    draw' (TNode i x) = concat [show i, "───{", show x, "}"] : []
    draw' (TForest i []) = [show i]
    draw' (TForest i [x]) = shift (show i ++ "───") ((show i *> " ") ++ "   ") (draw' x)
    draw' (TForest i (x:xs)) = (shift (show i ++ "─┬─") ((show i *> " ") ++ " │ ") (draw' x)) ++ (draw'' (show i *> " ") xs)
    draw'' p [x] = shift (p ++ " └─") (p ++ "   ") (draw' x)
    draw'' p (x:xs) = (shift (p ++ " ├─") (p ++ " │ ") (draw' x)) ++ (draw'' p xs)
    shift first other = zipWith (++) (first : repeat other)
    unlines' [x] = x
    unlines' [] = []
    unlines' (x:xs) = x ++ ('\n':(unlines' xs))

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

-- instance Foldable (TTree a) where
    -- foldr = foldrT

foldrT f x (TNode _ y) = f y x
foldrT f x (TForest _ ys) = foldr (foldrT f) x ys

instance Show (IO a) where
    show _ = "()"
type Binds = TTree Int (IO ())

u = TNode "u0" 1
v = TForest "v0" [TNode "v00" 2, TNode "v01" 3]
w = TForest "w0" [TForest "w00" [TNode "000" 4, TNode "001" 5], TNode "w01" 6, TForest "w02" [TForest "w020" [TNode "w0200" 7]], TForest "w03" []]
