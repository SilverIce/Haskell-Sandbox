module BTreeTest (
    test
)
where

import qualified Data.List as List
--import qualified Data.Maybe as Maybe

import qualified BTree as BT

--import qualified Control.Monad as CM
import Data.List.Split (splitWhen)

maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f (Just a) = Just $ f a
maybeApply f Nothing  = Nothing

maybeApply2 :: a -> Maybe (a -> b) -> Maybe b
maybeApply2 a (Just f) = Just $ f a
maybeApply2 _ Nothing = Nothing

type BTreeCommand a = [String] -> BT.BTree a -> BT.BTree a

getCommand :: (Ord a, Read a) => String -> Maybe (BT.BTree a -> BT.BTree a)
getCommand command = do
        let (com:args) = splitWhen (' ' ==) command
        handler <- getHandler com
        return (handler args)

getHandler :: (Ord a, Read a) => String -> Maybe (BTreeCommand a)
getHandler command = do
    (_,handler) <- List.find (\(s,_) -> s == command) handlers
    return handler

handlers :: (Ord a, Read a) => [(String, BTreeCommand a)]
handlers = [
              pair "fromList" (const . BT.fromList . get0)
            , pair "insert" (BT.insert . get0)
            , pair "remove" (BT.remove . get0)
            , pair "merge" (BT.merge . BT.fromList . get0)
            , pair "rebalance" (const BT.rebalance)
            ]
            where pair a b = (a, b)
                  get0 (a:_) = read a


test :: IO ()
test = do
    let ls = [1,-9,3,8,2,5,100]
        tree = BT.fromList ls
    putStrLn $ "given BTree " ++ show tree
    putStrLn $ "available commands: " ++ foldr (\(p,_) s -> s ++ " " ++ p) "" (handlers :: [(String, BTreeCommand Int)])
    --args <- getArgs
    --putStrLn $ show args
    --putStrLn "Hello, let's construct BTree! Input any array of numbers, like [1,9,4]"
    --CM.forever $ do
    command <- getLine
    putStrLn $ "your input is: " ++ command
    let resultingTree = maybeApply2 tree (getCommand command)
    --let resultingTree = maybeApply2 tree (getCommand2 command)
    putStrLn $ "result is: " ++ show resultingTree



