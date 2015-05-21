module Main where

--import Helpers

import qualified Shapes
import qualified MonadProblem
import qualified BTree


map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f a = (f (head a)) : (map2 f (tail a))



main = do
	putStrLn BTree.test
	--putStrLn MonadProblem.testmain = do
	--putStrLn "Hello, what's your name?"
	--name <- getLine
	--putStrLn ("Hey " ++ name ++ ", you rock!")

