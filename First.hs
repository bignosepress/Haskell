module First (f) where

import Data.List

f :: Double -> Double
f x = 0

fact :: Double -> Double
fact 0 = 1
fact n = fact (n-1)

main = do
	putStrLn "Hello World" 
