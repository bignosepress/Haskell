module Functors where

import Prelude hiding (Just, Nothing)

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b
--    (<$) :: a -> f b -> f a

-- Functor Laws
-- fmap id      = id
-- fmap (g . h) = (fmap g) . (fmap h)


data MyMaybe a =  Nothing | Just a 
                        deriving (Eq, Show) 

instance Functor MyMaybe where
-- fmap (a -> b) -> MyMaybe a -> MyMaybe b
    fmap _  Nothing    =  Nothing
    fmap g (Just x)    =  Just (g x)

-- Functor laws check
-- fmap id (Just 6) = Just 6
-- ((fmap (+1)) . (fmap (*2))) (Just 2) = fmap ((+1) . (*2)) (Just 2) = Just 5
--

data List a = Empty 
              | Cons a (List a) deriving (Show) 


instance Functor List where
-- fmap (a -> b) -> List a -> List b
   fmap f Empty       = Empty
   fmap f (Cons x xs) = Cons (f x) (fmap f xs)  


data MyEither a b = Left1 a | Right1 b deriving (Show)

instance Functor (MyEither c) where
-- fmap (a -> b) -> MyEither c a -> MyEither c b
   fmap f (Left1 x ) = Left1 x
   fmap f (Right1 x) = Right1 (f x)


    





  
