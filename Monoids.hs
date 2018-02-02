module Monoids where

import Prelude hiding (Monoid(..))

newtype Prod a = Prod {getNumber :: a} deriving Show

class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

instance Monoid [a] where
    mempty  = []
    mappend = (++)

instance (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing
    m `mappend` Nothing = m
    Nothing `mappend` m = m
    Just x `mappend` Just y = Just (x `mappend` y)

instance (Num a) => Monoid (Prod a) where
    mempty = Prod 1
    mappend  p q =  Prod $ (getNumber p) * (getNumber q)


