module Main where

--import Helpers

import qualified Shapes
import qualified MonadProblem



map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f a = (f (head a)) : (map2 f (tail a))



main = putStrLn MonadProblem.test

