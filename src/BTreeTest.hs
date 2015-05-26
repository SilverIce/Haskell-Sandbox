module BTreeTest (
    test
)
where

import qualified Data.List       as List
import qualified Data.Maybe      as Maybe

import qualified BTree           as BT

--import qualified Control.Monad as CM
import           Data.List.Split (splitWhen)


type BTreeCommand a = [String] -> BT.BTree a -> BT.BTree a

getCommand :: (Ord a, Read a) => String -> Maybe (BT.BTree a -> BT.BTree a)
getCommand command = do
        let (com:args) = splitWhen (' ' ==) command
        handlerFunc <- fmap function (getHandler com)
        return $ handlerFunc args

getHandler :: (Ord a, Read a) => String -> Maybe (Handler a)
getHandler command = List.find ((== command) . name) handlers

data Handler a = Handler { name     :: String
                         , function :: BTreeCommand a
                         , comment  :: Maybe String
                         }

showCommands :: [Handler Int] -> String
showCommands handlers = foldr (\h s -> s ++ " " ++ name h ++ ": " ++ (Maybe.fromMaybe "" $ comment h) ++ "\n") "" handlers


handlers :: (Ord a, Read a) => [Handler a]
handlers = [
              pair "fromList" (const . BT.fromList . get0)
            , pair "insert" (BT.insert . get0)
            , pair "remove" (BT.remove . get0)
            , pair "merge" (BT.merge . BT.fromList . get0)
            , pair "rebalance" (const BT.rebalance)
            ]
            where pair a b = Handler { name=a, function=b, comment=Just "no comment yet" }
                  get0 (a:_) = read a



cycle' :: Monad m => (a -> m a) -> a -> m b
cycle' f x = f x >>= cycle' f

test :: IO ()
test = do
    --let ls = [1,-9,3,8,2,5,100]
    --    tree = BT.fromList ls
    --putStrLn $ "given BTree " ++ show tree

    putStrLn "Hello, let's construct BTree! Input any array of numbers, like [1,9,4]"
    ntree <- fmap (BT.fromList . (read :: String -> [Int])) getLine
    putStrLn $ "new tree: " ++ show ntree
    putStrLn $ "available commands: " ++ showCommands handlers

    cycle' (\t -> do
        command <- getLine
        putStrLn $ "your input is: " ++ command
        let resultingTree = fmap ($ t) (getCommand command)
        putStrLn $ "result is: " ++ show resultingTree
        return t) ntree
    --args <- getArgs
    --putStrLn $ show args
    --putStrLn "Hello, let's construct BTree! Input any array of numbers, like [1,9,4]"
    --CM.forever $ do
    --command <- getLine
    --putStrLn $ "your input is: " ++ command
    --let resultingTree = fmap ($ tree) (getCommand command)
    ----let resultingTree = maybeApply2 tree (getCommand2 command)
    --putStrLn $ "result is: " ++ show resultingTree



