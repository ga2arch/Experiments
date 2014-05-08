{-# LANGUAGE DeriveFunctor, StandaloneDeriving, UndecidableInstances #-}

import Data.List

data TreeF c f = EmptyF | LeafF c | NodeF f f
    deriving (Eq, Show, Functor)

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)

cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

hylo :: Functor f => (f c -> c) -> (a -> f a) -> a -> c
hylo f g = f . fmap (hylo f g) . g

unflatten :: Ord a => [a] -> TreeF a [a]
unflatten (  []) = EmptyF
unflatten (x:[]) = LeafF x
unflatten (x:xs) = NodeF l r   
  where
    l = (filter (<x) xs) ++ [x]
    r = filter (>=x) xs

flatten :: Ord a => TreeF a [a] -> [a] 
flatten EmptyF = []
flatten (LeafF x) = [x]
flatten (NodeF l r) = l ++ r

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists = curry $ unfoldr c 
  where
    c ([], []) = Nothing
    c ([], y:ys) = Just (y, ([], ys))
    c (x:xs, []) = Just (x, (xs, []))
    c (x:xs, y:ys) | x <= y = Just (x, (xs, y:ys))
                   | x > y = Just (y, (x:xs, ys))

msort :: Ord a => [a] -> [a]
msort = hylo flatten unflatten