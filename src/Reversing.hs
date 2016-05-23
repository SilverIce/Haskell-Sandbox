module Reversing (
    test
    ) where

import qualified Data.List       as List
import           Data.Ord        (comparing)
import           Data.List.Split (splitWhen)
import           Data.Maybe      as Maybe
import qualified Data.Text       as Text
import           Text.Read       (readMaybe)


type StreamPosition = Int
type FieldId = Int
type FieldAddress = Int
type Comment = String

data FieldType = Unk | Byte | GuidByte | Bit | DWord deriving (Enum, Show, Read)

data Field = Field {    fieldType       :: FieldType,
                        fieldAddress    :: FieldAddress,
                        streamPos       :: StreamPosition,
                        fieldId         :: FieldId,
                        comment         :: Maybe String
                   }

data InputField = InputField FieldType FieldAddress (Maybe Comment)
                  deriving (Show, Read)

type Fields = [Field]

type Command = String
type CommandArgs = String
type HandlerName = String
type CommandHandlerResult = (Fields, String)
type CommandHandler = CommandArgs -> Fields -> CommandHandlerResult

--data CommandHandlerEntry = CommandHandlerEntry {
--    name :: String,
--    description :: Maybe String,
--    handler :: CommandHandler
--}
    --deriving (Show)

instance Show Field where
    show f = "| x" ++ show (fieldAddress f)
                   ++ " " ++ show (fieldType f)
                   ++ " s" ++ show (streamPos f)
                   ++ " f" ++ show (fieldId f)


showFields :: Fields -> String
showFields = foldl (\ini fl -> ini ++ show fl ++ "\n") "\n"


parseField :: CommandArgs -> Maybe InputField
parseField input = readMaybe tupled where
    tupled = "InputField " ++ input

stringSplit :: Char -> String -> (String, String)
stringSplit c s = maybe (s, []) (dropSpace . (`splitAt` s)) (List.elemIndex c s)
                    where dropSpace (first, rest) = (first, drop 1 rest)


findHandler :: HandlerName -> CommandHandler
findHandler name = Maybe.fromMaybe zeroHandler (List.lookup name handlers)

fromInputField :: InputField -> Field
fromInputField (InputField typ addr comm) = Field {
    fieldAddress = addr
    , fieldType = typ
    , comment = comm
    , fieldId = -1
    , streamPos = -1
    }

pushField :: Fields -> InputField -> Fields
pushField fields@(prev:_) fld = (fromInputField fld) { streamPos = 1 + streamPos prev} : fields
pushField [] fld = [(fromInputField fld) { streamPos = 0 }]

zeroFields :: Fields
zeroFields = []

--------------------

type FieldGroupCounter = Int
type CondenseFieldRule = FieldGroupCounter -> Fields -> (Fields, Fields)
type RuleGetter = Fields -> Maybe CondenseFieldRule

guidRule :: Fields -> Maybe CondenseFieldRule
guidRule [] = Nothing
guidRule fs = if isGuid fs then Just condenseGuid else Nothing
    where
        -- length >= 16
        isGuid fs = False
        condenseGuid _ (f:fs) = ([f], fs)
        match (Field GuidByte _ _ _ _):(Field Bit _ _ _ _) = id


zeroRule :: Fields -> Maybe CondenseFieldRule
zeroRule _ = Just $ \id (f:fs) -> ([handleFld id f], fs)
    where handleFld id fl = fl {fieldId = id}

findCondenseRule :: [RuleGetter] -> Fields -> CondenseFieldRule
--findCondenseRule rules fields = zeroRule fields  -- mock
findCondenseRule rules fields = fromJust . fromJust $ findRule rules fields
    where
        findRule rules fields = List.find isJust $ map (\getter -> getter fields) rules


condenseRules :: [RuleGetter]
condenseRules = [guidRule, zeroRule]

condenseFields :: Fields -> Fields
--condenseFields fs = fst $ (findCondenseRule condenseRules fs) 0 fs
condenseFields fs = make 0 ([], sortByAdress fs)
    where
        sortByAdress = List.sortBy (comparing fieldAddress)
        make _       (f,[]) = f
        make counter (f,s) = f ++ make (counter + 1) (findRule (counter + 1) s)
        --make _       (f, []) = f
        findRule counter fields = (findCondenseRule condenseRules fields) counter fields

------------------------------------

zeroHandler :: Command -> Fields -> CommandHandlerResult
zeroHandler _ fields = (fields, "no handler found")

handlers :: [(HandlerName, CommandHandler)]
handlers = [
            pair "add" $ makeHandler pushField parseField "can't parse input",
            pair "struct" $ makeShowHandler (List.sortBy (comparing fieldAddress) ),
            pair "print" $ makeShowHandler id,
            pair "reset" $ \_ _ -> (zeroFields, ""),
            pair "help" $ \_ fs -> (fs, show $ map fst handlers),
            pair "condense" $ \_ fs -> (condenseFields fs, "")
        ]
        where pair a b = (a, b)
              idk f s (Just m) _ = pair (f s m) ""
              idk _ s Nothing e = pair s e
              makeHandler f p e = \args fields -> idk f fields (p args) e
              makeShowHandler f = \_ fields -> (fields, showFields $ f fields)
              --idk _ s _ _ = (s, "input is not a Maybe monad"

cycle' :: Monad m => (a -> m a) -> a -> m b
cycle' f x = f x >>= cycle' f


test ::  IO ()
test = do
    putStrLn "Hi"
    putStrLn "Now input your damn commands"
    --putStrLn . show $ Field 0 Unk 0
    --line <- getLine
    --let line = "14 Bit"
    let fields = zeroFields
    --putStrLn $ "field is: " ++ show fields
    putStrLn $ show (InputField Byte 0 (Just ""))
    let x = "(Byte, 1, Just \"toto\")"
    putStrLn . show $ (read :: String -> (FieldType,Int,Maybe String)) x

    cycle' (\t -> do
        command <- getLine
        putStrLn $ "input: " ++ command
        let (cmdName, restOfCmd) = stringSplit ' ' command
        --putStrLn $ "split " ++ show (cmdName, restOfCmd)
        let handler = findHandler cmdName
        let (newState, cmdout) = handler restOfCmd t
        putStrLn $ "cmdout: " ++ cmdout
        putStrLn $ "new state: " ++ showFields newState
        return newState) fields











