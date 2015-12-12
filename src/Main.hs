module Main where

import qualified MonadProblem
import qualified BTreeTest
import qualified Reversing
import Control.Applicative

sqrt' :: Float -> Maybe Float
sqrt' v 
    | v >= 0 = Just $ sqrt v
    | otherwise = Nothing

xx :: Float -> Float
xx v = 3.0 * v

trololo :: Maybe Float -> Maybe Float
trololo x = (return 5.0) >>= sqrt' >>= return . xx 

main = 
    --putStrLn $ show $ Just (3+) <*> Just 4
    --BTreeTest.test
    Reversing.test



