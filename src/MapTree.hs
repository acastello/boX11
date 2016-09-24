module MapTree where

import qualified Data.Map as M
import Data.List (intersperse)

data MapTree k a =
      Branch (M.Map k (MapTree k a))
    | Leaf a
    deriving Eq

infixr 5 .<
k .< xs = (k, Branch $ M.fromList xs)

infixr 5 .>
k .> a = (k, Leaf a)

instance (Show a, Show b) => Show (MapTree a b) where
    show (Leaf a) = concat ["[", show a, "]"]
    show (Branch m) = concat $ ["["] ++ (intersperse ", " $ show' <$> (M.toList m)) ++ ["]"]
        where show' (k,a) = show k ++ " -> " ++ show a

drawMapTree :: (Show k, Show a) => MapTree k a -> String
drawMapTree mt = unlines $ firstdraw mt where
    firstdraw (Leaf a) = concat ["{", show a, "}"] :[]
    firstdraw (Branch m) = firstdraw' (M.toList m)
    firstdraw' [] = ["()"]
    firstdraw' [(x,m)] = shift (show x ++ "─") (show x ++ " " *> " ") (draw m)
    firstdraw' ((x,m):xs) = (shift ("┌─" ++ show x ++ "─") ("│ " ++ (show x ++ "-" *> " ")) (draw m)) ++ (firstdraw'' xs)
    firstdraw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("└─" ++ show x ++ "─" *> " ") (draw m)
    firstdraw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (firstdraw'' xs)
    draw (Leaf a) = concat ["──{", show a, "}"] :[]
    draw (Branch m) = draw' $ M.toList m
    draw' [] = "()":[]
    draw' [(x,m)] = shift ("──" ++ show x ++ "─") ("--" ++ show x ++ "-" *> " ") (draw m)
    draw' ((x,m):xs) = shift ("┬─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    draw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("  " ++ show x ++ " " *> " ") (draw m) 
    draw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    shift first other = zipWith (++) (first : repeat other)

instance Ord k => Monoid (MapTree k b) where
    mempty = Branch M.empty
    (Branch m1) `mappend` (Branch m2) = Branch $ M.unionWith mappend m1 m2
    fst `mappend` _ = fst

instance Functor (MapTree k) where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch m) = Branch $ (fmap f) <$> m
    
instance Applicative (MapTree k) where
    pure a = Leaf a
    Leaf f <*> t = f <$> t       
    Branch m <*> Leaf a = ($ a) <$> Branch m
    Branch m <*> Branch m' = Branch $ (Branch m <*>) <$> m'
    
instance Monad (MapTree k) where
    return = pure
    Leaf a >>= f = f a
    Branch m >>= f = Branch $ (>>= f) <$> m
    
    
mapKeys :: Ord k2 => (k1 -> k2) -> MapTree k1 a -> MapTree k2 a
mapKeys f (Leaf a) = Leaf a
mapKeys f (Branch m) = Branch $ (mapKeys f) <$> (M.mapKeys f m)

instance Foldable (MapTree k) where
    foldr = fold

fold :: (a -> b -> b) -> b -> MapTree k a -> b
fold f b (Leaf a) = f a b
fold f b (Branch m) = foldr (flip $ fold f) b m

instance Traversable (MapTree k) where
    traverse f (Leaf a) = Leaf <$> f a    
    traverse f (Branch m) = Branch <$> (traverse (traverse f) m)

mapKeysM :: (Ord k2, Monad m) => (k1 -> m k2) -> MapTree k1 a -> m (MapTree k2 a)
mapKeysM f (Leaf a) = return (Leaf a)
-- mapKeysM f (Branch m) = 
-- traverseK :: Applicative t => (k1 -> t k2) -> M.Map k1 a -> t (M.Map k2 a)
traverseK f m = 

fromList :: Ord k => [(k,MapTree k a)] -> MapTree k a
fromList = Branch . M.fromList

toList :: MapTree k a -> [a]
toList = foldr (:) []

lookup :: Ord k => k -> MapTree k a -> Maybe (Either (MapTree k a) a)
lookup k (Leaf _) = Nothing
lookup k (Branch m) = Left <$> M.lookup k m 

(!) :: Ord k => MapTree k a -> [k] -> a
(Leaf a)![] = a
(Leaf a)!_ = error "too many keys provided"
(Branch m)!(k:ks) = case m M.! k of
    (Leaf a') -> a'
    b -> b!ks
(Branch m)![] = error "not enough keys"
    

member :: Ord k => k -> MapTree k a -> Bool
member k (Branch m) = M.member k m
member _ _ = False





test1 = fromList
    [ 1 .<
        [ 1 .> 'a'
        , 2 .< 
            [ 1 .> 'b'
            , 2 .> 'c']
        , 3 .< 
            [ 4 .> 'd' ] ]
    , 2 .> 'e' ]
        
test2 = fromList
    [ 1.< [ 2 .> 'a' ]]

test3 = fromList
    [ 1.< 
        [ -2 .> 'a' ]
    , 3 .> 'b'
    , 4 .< [] ] 

test4 = fromList
    [ 1 .> (:[]) ]

test5 = fromList
    [ -1 .> (:[])
    , -2 .> (:"-d")]
