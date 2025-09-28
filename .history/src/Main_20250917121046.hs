{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}


import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (finally)
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=), encode, eitherDecode, FromJSON, ToJSON, parseJSON, toJSON, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeFileName, takeExtension)
import Web.Scotty
import qualified Web.Scotty as Scotty

type ClientId = Int
type Client = (ClientId, Connection)

data AppState = AppState
  { userCount :: Int
  , messages  :: [String]
  , clients   :: [Client]
  , nextClientId :: ClientId
  } deriving (Show)

data ChannelMessage 
  = UserMessage String
  | SystemMessage String
  | UserJoined ClientId
  | UserLeft ClientId
  deriving (Show, Generic)

instance ToJSON ChannelMessage where
  toJSON (UserMessage msg) = object
    [ "type" .= ("user_message" :: String)
    , "message" .= msg
    ]
  toJSON (SystemMessage msg) = object
    [ "type" .= ("system_message" :: String)
    , "message" .= msg
    ]
  toJSON (UserJoined cid) = object
    [ "type" .= ("user_joined" :: String)
    , "client_id" .= cid
    ]
  toJSON (UserLeft cid) = object
    [ "type" .= ("user_left" :: String)
    , "client_id" .= cid
    ]

data WSMessage = WSMessage
  { wsType :: String
  , wsMessage :: String
  } deriving (Show, Generic)

instance FromJSON WSMessage where
  parseJSON = withObject "WSMessage" $ \o -> WSMessage
    <$> o .: "type"
    <*> o .: "message"

instance ToJSON WSMessage where
  toJSON (WSMessage t m) = object
    [ "type" .= t
    , "message" .= m
    ]

data NewDataType = NewDataType
  { message :: String
  , effect :: String
  } deriving (Show, Generic)  

data ModifiedJson = ModifiedJson 
  { onemessage :: String
  , status :: String
  } deriving (Show, Generic)

-- general
data IncomingData = IncomingData
  { dataMessage :: String
  , dataType :: String  
  , dataAuthcode :: String
  } deriving (Show, Generic)

-- general
data Incoming = Incoming
  { kind :: String
  , incomingData :: IncomingData
  } deriving (Show, Generic)

--new user
data IncomingUser = IncomingUser
  { user :: String
  , password :: String
  , user_perms :: [String]
  } deriving (Show, Generic)

--new user
data Element = Element
  { kind :: String
  , incomingUser :: IncomingUser
  } deriving (Show, Generic)

--wraped for the two kinds of kind
data Wrapped a = Wrapped
  { kind :: String
  , data_ :: a
  } deriving (Show, Generic)

instance FromJSON NewDataType where
  parseJSON = withObject "NewDataType" $ \o -> NewDataType
    <$> o .: "message"
    <*> o .: "effect"

instance ToJSON NewDataType where
  toJSON (NewDataType m e) = object
    [ "message" .= m
    , "effect" .= e
    ] 


instance FromJSON ModifiedJson where
  parseJSON = withObject "ModifiedJson" $ \o -> ModifiedJson
    <$> o .: "onemessage"
    <*> o .: "status"

instance ToJSON ModifiedJson where
  toJSON (ModifiedJson m s) = object
    [ "onemessage" .= m
    , "status" .= s
    ]

instance FromJSON IncomingData where
  parseJSON = withObject "IncomingData" $ \o -> IncomingData
    <$> o .: "message"
    <*> o .: "type"
    <*> o .: "authcode"

instance ToJSON IncomingData where
  toJSON (IncomingData m t a) = object
    [ "message" .= m
    , "type" .= t
    , "authcode" .= a
    ]

instance FromJSON Incoming where
  parseJSON = withObject "Incoming" $ \o -> Incoming
    <$> o .: "kind"
    <*> o .: "data"

instance ToJSON Incoming where
  toJSON (Incoming k d) = object
    [ "kind" .= k
    , "data" .= d
    ]

--newuser
instance FromJSON IncomingUser where
  parseJSON = withObject "IncomingUser" $ \o -> IncomingUser
    <$> o .: "user"
    <*> o .: "password"
    <*> o .: "user_perms"

instance ToJSON IncomingUser where
  toJSON (IncomingUser u p s) = object
    [ "user" .= u
    , "password" .= p
    , "user_perms" .= s
    ]

--newuser
instance FromJSON Element where
  parseJSON = withObject "Element" $ \o -> Element
    <$> o .: "kind"
    <*> o .: "data"

instance ToJSON Element where
  toJSON (Element k d) = object
    [ "kind" .= k
    , "data" .= d
    ]

--wraped
instance (FromJSON a) => FromJSON (Wrapped a) where
  parseJSON = withObject "Wrapped" $ \o ->
    Wrapped <$> o .: "kind"
            <*> o .: "data"

--wraped
instance (ToJSON a) => ToJSON (Wrapped a) where
  toJSON (Wrapped k d) = object
    [ "kind" .= k
    , "data" .= d
    ]



generalJSON :: String -> String -> String -> String -> Value
generalJSON kind type_ message authcode =
 object
 [ "kind" .= kind
 , "data" .= object
   [ "type" .= type_
   , "message" .= message
   , "authcode" .= authcode
   ]
 , "response" .= ("Command processed successfully" :: String)
 ]
-- main!
mainHandler :: ActionM ()
mainHandler = do
  bodyText <- body
  liftIO $ putStrLn $ "Raw body received: " ++ show (BL.take 200 bodyText)

  let decoded = eitherDecode bodyText :: Either String (Wrapped IncomingData)

  case decoded of
    Left err -> do
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      liftIO $ putStrLn $ "Body length: " ++ show (BL.length bodyText)
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Wrapped kind data_) -> do
      let jsonVal = generalJSON kind
                                (dataType data_)
                                (dataMessage data_)
                                (dataAuthcode data_)
          jsonBytes = encode jsonVal
          jsonString = BL.unpack jsonBytes

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind data_)
      liftIO $ putStrLn $ "Responding with: " ++ jsonString
      json jsonVal

testHandler :: ActionM ()
testHandler = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String ModifiedJson
  case decoded of
    Left err -> do  -- Fixed: was 'left err'
      liftIO $ putStrLn $ "Invalid JSON: " ++ err
      json $ object
        [ "onemessage" .= ("Invalid JSON: " ++ err)
        , "status" .= ("Error" :: String)
        ]
    Right incoming -> do  -- Fixed: was 'right incoming'
      liftIO $ putStrLn $ "Received valid JSON: " ++ show incoming
      json $ object
          [ "onemessage" .= ("Hello world! TM" :: String)
          , "status" .= ("success" :: String)
          ]
--returning json /!
newDataHandler :: ActionM ()
newDataHandler = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String NewDataType
  case decoded of
    Left err -> do
      liftIO $ putStrLn $ "Json Error: " ++ err
      json $ object
        [ "message" .= ("no effect found, invalid json" :: String)
        , "effect"  .= ("none" :: String)
        ]
    Right incoming -> do
      liftIO $ putStrLn $ "Received valid Json: " ++ show incoming
      let modified = incoming { effect = "strengthened-" ++ effect incoming }
      json modified

pracHandler :: IORef AppState -> ActionM ()
pracHandler stateRef = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String (Wrapped IncomingData)
  case decoded of 
    Left err -> do
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      liftIO $ putStrLn $ "Body length: " ++ show (BL.length bodyText)
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Wrapped kind data_) -> do
      let jsonVal = generalJSON kind
                                (dataType data_)
                                (dataMessage data_)
                                (dataAuthcode data_)

          jsonBytes = encode jsonVal
          jsonString = BL.unpack jsonBytes

      liftIO $ modifyIORef stateRef (\s -> s { messages = messages s ++ [dataMessage data_] })

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind data_)
      liftIO $ putStrLn $ "Responding with: " ++ jsonString
      json jsonVal

postHandler :: ActionM ()
postHandler = do
 bodyText <- body
 let modifiedStr = map toUpper (BL.unpack bodyText)
 text $ TL.pack ("Processed: " ++ modifiedStr)

--not finnished yet
newUserHandler :: ActionM ()
newUserHandler = do
 bodyText <- body
 let decoded = eitherDecode bodyText :: Either String IncomingUser
 json decoded

getMimeType :: String -> String
getMimeType filename = case takeExtension filename of
  ".html" -> "text/html; charset=utf-8"
  ".css"  -> "text/css; charset=utf-8"
  ".js"   -> "application/javascript; charset=utf-8"
  ".json" -> "application/json; charset=utf-8"
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".gif"  -> "image/gif"
  ".svg"  -> "image/svg+xml"
  ".ico"  -> "image/x-icon"
  ".txt"  -> "text/plain; charset=utf-8"
  _       -> "application/octet-stream"

makeHtmlHandler :: FilePath -> ActionM ()
makeHtmlHandler filepath = do
 content <- liftIO $ TLIO.readFile filepath
 let replaced = TL.replace "[[SITE_URL]]" "/static" content
 html replaced

makeStaticHandlers :: FilePath -> ScottyM ()
makeStaticHandlers dir = do
 get "/static/:filename" $ do
   filename <- param "filename"
   let path = dir ++ "/" ++ filename
   isFile <- liftIO $ doesFileExist path
   if isFile
     then if ".html" `isSuffixOf` filename
            then makeHtmlHandler path
            else do
              content <- liftIO $ TLIO.readFile path
              setHeader "Content-Type" (TL.pack $ getMimeType filename)
              text content
     else text "File not found"

main :: IO ()
main = do
 putStrLn "Scotty server running on port 7879..."
 stateRef <- newIORef (AppState 0 [])
 scotty 7879 $ do
   makeStaticHandlers "public"
   get "/api/test" $ text "Hello world api/test"  -- Fixed: was 'test "Hello world"'
   get "/api/message" $ json $ object
     ["response" .= object
      [ "message" .= ("Hello, world" :: String)
      , "origin" .= ("0" :: String)
      ]
     ]
   post "/api/general" mainHandler
   post "/api/test"  postHandler
   post "/api/prac" (pracHandler stateRef)
   post "/api/practice" testHandler
   post "/api/newdata" newDataHandler
   post "/api/createuser" newUserHandler