module MapTree where

import qualified Data.Map as M
import Data.List (intersperse)

data MapTree k a =
      Branch (M.Map k (MapTree k a))
    | Leaf a
    deriving Eq

instance (Show a, Show b) => Show (MapTree a b) where
    show (Leaf a) = "{" ++ show a ++ "}"
    show (Branch m) = concat $ ["{"] ++ (intersperse "," $ show' <$> (M.toList m)) ++ ["}"]
        where show' (k,a) = show k ++ "-" ++ show a
        
