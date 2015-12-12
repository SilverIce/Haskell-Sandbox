module Reversing (
    test
    ) where

import qualified Data.List       as List
import           Data.Ord        (comparing)
import           Data.List.Split (splitWhen)
import qualified Data.Maybe      as Maybe

data FieldType = Unk | Byte | GuidByte | Bit | DWord deriving (Enum, Show, Read)

data Field = Field {    fieldAddress    :: Int,
                        fieldType       :: FieldType,
                        streamPos       :: Int
                   }
                   deriving (Show)

type Fields = [Field]

type Command = String
type CommandArgs = String
type HandlerName = String
type CommandHandlerResult = (Fields, String)
type CommandHandler = CommandArgs -> Fields -> CommandHandlerResult


parseField :: CommandArgs -> Maybe Field
parseField input = Just $ Field (read sAddr :: Int) (read sType :: FieldType) 0
    where
        (sAddr:sType:_) = splitWhen (' ' ==) input





findHandler :: HandlerName -> Maybe CommandHandler
findHandler name = List.lookup name handlers

pushField :: Fields -> Field -> Fields
pushField fields@(prev:_) fld = fld { streamPos = 1 + streamPos prev} : fields
pushField [] fld = fld { streamPos = 0 } : []

handlers :: [(HandlerName, CommandHandler)]
handlers = [

            pair "add" $ makeHandler pushField parseField "can't parse input",
            pair "struct" $ makeShowHandler (List.sortBy (comparing fieldAddress) ),
            pair "print" $ makeShowHandler id
        ]
        where pair a b = (a, b)
              idk f s (Just m) e = pair (f s m) ""
              idk f s Nothing e = pair s e
              makeHandler f p e = \args fields -> idk f fields (p args) e
              makeShowHandler f = \_ fields -> (fields, show $ f fields)
              --idk _ s _ _ = (s, "input is not a Maybe monad")

cycle' :: Monad m => (a -> m a) -> a -> m b
cycle' f x = f x >>= cycle' f


test ::  IO ()
test = do
    --putStrLn "Hi"
    --putStrLn "Now input your damn commands"
    --putStrLn . show $ Field 0 Unk 0
    --line <- getLine
    let line = "14 Bit"
    let fields = [parseField line]
    putStrLn $ "field is: " ++ show fields

    --cycle' (\t -> do
    --    command <- getLine
    --    putStrLn $ "cmd:" ++ command
    --    return t) fields
