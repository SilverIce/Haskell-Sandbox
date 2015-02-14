module BTree where


data BTree a = Empty | Node a (BTree a, BTree a) deriving (Show)

tell :: (Show a) => BTree a -> String
tell (Empty) = ""
tell (Node a (l, r)) = (show a) ++ " {L:" ++ tell l ++ "," ++ tell r ++ ":R}"

leaf a = Node a (Empty, Empty)

--Node a (Leaf l, Leaf r) = Node a (Leaf r, Leaf l)

nodeCompare :: (Ord a) => BTree a -> a -> Ordering
nodeCompare Empty _ = LT
nodeCompare (Node a _) v = compare a v

contains :: (Ord a) => BTree a -> a -> Bool
contains Empty _ = False
contains (Node a (l, r)) v = case v `compare` a of EQ -> True
                                                   GT -> contains r v
                                                   LT -> contains l v

insert :: (Ord a) => a -> BTree a -> BTree a
insert v Empty = leaf v
insert v (Node a (l, r))
	| v == a = Node a (l, r)
	| v > a = Node a (l, insert v r)
	| otherwise = Node a (insert v l, r)


test :: String
--test = show $ contains (Node 1 (Empty, leaf 2)) 2
test = 
	let tree = foldr insert Empty [1,2,3]
	in show . tell $ tree
