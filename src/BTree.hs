{-# LANGUAGE RankNTypes #-}
module BTree (
    BTree(Empty, Node),

    fromList,
    fromSortedUniqueList,

    contains,
    valueList,
    merge,
    remove,
    insert,
    validate,
    rebalance,

    test,
) where

import qualified Data.List as List

data BTree a = Empty | Node a (BTree a, BTree a)

instance (Show a) => Show (BTree a) where
    show t = '`' : (tell t) ++ "`"

tell :: (Show a) => BTree a -> String
tell (Empty) = "_"
tell (Node a (Empty, Empty)) = show a
tell (Node a (l, r)) = (show a) ++ " {" ++ tell l ++ " " ++ tell r ++ "}"

leaf :: a -> BTree a
leaf a = Node a (Empty, Empty)

contains :: (Ord a) => BTree a -> a -> Bool
contains Empty _ = False
contains (Node a (l, r)) v = case v `compare` a of EQ -> True
                                                   GT -> contains r v
                                                   LT -> contains l v
-- returns sorted list of values
valueList :: BTree a -> [a]
valueList Empty = []
valueList (Node a (l, r)) = valueList l ++ [a] ++ valueList r


insert :: (Ord a) => a -> BTree a -> BTree a
insert v Empty = leaf v
insert v (Node a (l, r)) = case v `compare` a of EQ -> Node a (l, r)
                                                 GT -> Node a (l, insert v r)
                                                 LT -> Node a (insert v l, r)


merge :: (Ord a) => BTree a -> BTree a -> BTree a
merge l Empty = l
merge Empty r = r
merge l r = fromList $ (valueList l) ++ (valueList r)

remove :: (Ord a) => a -> BTree a -> BTree a
remove _ Empty = Empty
remove v (Node a (l, r)) = case v `compare` a of GT -> Node a (l, remove v r)
                                                 LT -> Node a (remove v l, r)
                                                 EQ -> merge l r
                              
validate :: (Ord a) => BTree a -> Bool
validate Empty = True
validate (Node v (l, r)) = (cmp v (>) l) && (cmp v (<) r) && validate l && validate r
                                where   cmp a f (Node nv _) = a `f` nv
                                        cmp _ _ Empty = True

rebalance :: (Ord a) => BTree a -> BTree a
rebalance tree = fromSortedUniqueList $ valueList tree

fromList :: (Ord a) => [a] -> BTree a
fromList ar = fromSortedUniqueList . List.nub . List.sort $ ar

fromSortedUniqueList :: (Ord a) => [a] -> BTree a
fromSortedUniqueList [] = Empty
fromSortedUniqueList [x] = leaf x
fromSortedUniqueList a = _fromSortedUniqueList a (length a)

_fromSortedUniqueList [] _ = Empty
_fromSortedUniqueList [x] _ = leaf x
_fromSortedUniqueList sa l = Node rh (_fromSortedUniqueList la left, _fromSortedUniqueList rt right) where
    left = l `div` 2
    right = l - left - 1
    (la, (rh:rt)) = splitAt left sa


testContains :: (Show a, Ord a) => BTree a -> String
testContains tree = show $ map f (valueList tree)
                where f v = "contains " ++ (show v) ++ " = " ++ (show $ contains tree v)


-- op. name, args, resulting tree
uniTest0 :: Show b => [Char] -> (a -> b) -> a -> [Char]
uniTest0 opname func = ((opname ++ ": ") ++) . show . func

uniTest1 :: (Show b, Show a1) =>
              [Char] -> (a1 -> a -> b) -> a1 -> a -> [Char]
uniTest1 opname func a1 = ((opname ++ " " ++ show a1 ++ ": ") ++) . show . (func a1)

testTree :: (Ord a, Show a) => BTree a -> [BTree a -> String] -> String
testTree tree functions = "given a tree " ++ show tree ++ ":\n" ++ showLst functions
                        where showLst l = foldr (\f s -> f tree ++ "\n" ++ s) "" l

test :: String
test = 
    let ls = [1,-9,3,8,2,5,100]
        tree = fromList ls
    in testTree tree tests
        where
            tests = [testContains,
                    insr 10,
                    insr 1,
                    insr 2,
                    rm 2,
                    rm 8,
                    uniTest1 "merge" (merge . fromList) [2,5,100],
                    uniTest0 "flattern" valueList
                    ]
            insr = uniTest1 "insert" insert
            rm = uniTest1 "remove" remove

