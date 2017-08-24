module Applicatives where

import Functors

-- class Functor f => Applicative f where
--   pure   :: a -> f a
--   (<*>)  :: f (a -> b) -> f a -> f b
--
--   Applicative Laws
--
--   The identity Law
--   pure id <*> v = v
--
--   Homomorphism
--   pure f <*> pure x = pure (f x)
--
--   Interchange
--   u <*> pure y = pure ($ y) <*> u
--
--   Composition
--   u <*> (v <*> w) = pure (.) <*> u  <*> v <*> w
--
--   g <$> x = pure g <*> x (fmap expressed in applicative terms)
--
instance Applicative MyMaybe where
    pure  = MyJust
    (<*>) (MyJust f) x = fmap f x
    (<*>) MyNothing _  = MyNothing
