module Utils where

--import qualified Haskell.Functions as F



myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = [f x] ++ (myMap f xs)

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 _ [] = []
myMap2 f (x:xs) = (f x) : myMap2 f xs

getLen' :: [a] -> Int ->  Int
getLen' [] acc = acc
getLen' (x:xs) acc = getLen' xs (acc + 1)    

getLen :: [a] -> Int
getLen xs = getLen' xs 0

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init (x:xs) = myFoldl f (f init x) xs
myFoldl _ init [] = init 

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f init (x:xs) = f x (myFoldr f init xs)
myFoldr _ init [] = init

main :: IO()
main = do
    putStrLn "Recursion"



