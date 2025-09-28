{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}


import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=), encode, eitherDecode, FromJSON, ToJSON, parseJSON, toJSON, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeFileName, takeExtension)
import Web.Scotty

data AppState = AppState
  { userCount :: Int
  , messages  :: [String]
  , broadcastMessages :: [String]  
  } deriving (Show)

data ChannelMessage 
  = UserMessage String
  | SystemMessage String
  | UserJoined Int
  | UserLeft Int
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
-- ModifyElementData you can find in the users.js, look at the json sent and recreate the datatype
-- All the user data will concern the user array in AppState, examples are prac handler on how to modify data in the 
-- app state

-- /api/users (GET)
-- A list of users, will use the DB tm

-- /api/edituser (POST)
-- send a full user object, it will take a ModifyElementData which includes the user data inside the element feild, use the 
-- username to find the user, then edit the other feilds based off that

-- /api/getuser (POST)
-- gets the user object based on the name given

-- data ChannelMessage 
--   = UserMessage String
--   | SystemMessage String
--   | UserJoined ClientId
--   | UserLeft ClientId
--   deriving (Show, Generic)

-- instance ToJSON ChannelMessage where
--   toJSON (UserMessage msg) = object
--     [ "type" .= ("user_message" :: String)
--     , "message" .= msg
--     ]
--   toJSON (SystemMessage msg) = object
--     [ "type" .= ("system_message" :: String)
--     , "message" .= msg
--     ]
--   toJSON (UserJoined cid) = object
--     [ "type" .= ("user_joined" :: String)
--     , "client_id" .= cid
--     ]
--   toJSON (UserLeft cid) = object
--     [ "type" .= ("user_left" :: String)
--     , "client_id" .= cid
--     ]

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


-- Simplified broadcast function using MVar
addBroadcastMessage :: MVar AppState -> String -> IO ()
addBroadcastMessage stateVar msg = do
  modifyMVar_ stateVar $ \state -> return state 
    { broadcastMessages = broadcastMessages state ++ [msg] }

-- Function to get and clear broadcast messages (for polling-based approach)
getBroadcastMessages :: MVar AppState -> IO [String]
getBroadcastMessages stateVar = do
  state <- readMVar stateVar
  modifyMVar_ stateVar $ \s -> return s { broadcastMessages = [] }
  return (broadcastMessages state)

-- Your existing functions
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
 
mainHandler :: MVar AppState -> ActionM ()
mainHandler stateVar = do
  bodyText <- body
  liftIO $ putStrLn $ "Raw body received: " ++ show (BL.take 200 bodyText)

  let decoded = eitherDecode bodyText :: Either String (Wrapped IncomingData)

  case decoded of
    Left err -> do
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Wrapped kind data_) -> do
      let jsonVal = generalJSON kind
                                (dataType data_)
                                (dataMessage data_)
                                (dataAuthcode data_)

      liftIO $ do
        modifyMVar_ stateVar $ \state -> return state 
          { messages = messages state ++ [dataMessage data_] }
        addBroadcastMessage stateVar ("API: " ++ dataMessage data_)

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind data_)
      json jsonVal

testHandler :: ActionM ()
testHandler = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String ModifiedJson
  case decoded of
    Left err -> do
      liftIO $ putStrLn $ "Invalid JSON: " ++ err
      json $ object
        [ "onemessage" .= ("Invalid JSON: " ++ err)
        , "status" .= ("Error" :: String)
        ]
    Right incoming -> do
      liftIO $ putStrLn $ "Received valid JSON: " ++ show incoming
      json $ object
          [ "onemessage" .= ("Hello world! TM" :: String)
          , "status" .= ("success" :: String)
          ]

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

pracHandler :: MVar AppState -> ActionM ()
pracHandler stateVar = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String (Wrapped IncomingData)
  case decoded of 
    Left err -> do
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Wrapped kind data_) -> do
      let jsonVal = generalJSON kind
                                (dataType data_)
                                (dataMessage data_)
                                (dataAuthcode data_)

      liftIO $ do
        modifyMVar_ stateVar $ \state -> return state 
          { messages = messages state ++ [dataMessage data_] }
        addBroadcastMessage stateVar ("Practice: " ++ dataMessage data_)

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind data_)
      json jsonVal

messagesHandler :: MVar AppState -> ActionM ()
messagesHandler stateVar = do
  msgs <- liftIO $ getBroadcastMessages stateVar
  state <- liftIO $ readMVar stateVar
  json $ object 
    [ "messages" .= msgs
    , "userCount" .= userCount state
    , "allMessages" .= messages state
    ]

postHandler :: ActionM ()
postHandler = do
 bodyText <- body
 let modifiedStr = map toUpper (BL.unpack bodyText)
 text $ TL.pack ("Processed: " ++ modifiedStr)

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
  putStrLn "Starting server with polling-based messaging on port 7879..."
  
  stateVar <- newMVar (AppState 0 [] [])
  
  scotty 7879 $ do
    makeStaticHandlers "public"
    get "/api/test" $ text "Hello world api/test"
    get "/api/message" $ json $ object
      ["response" .= object
       [ "message" .= ("Hello, world" :: String)
       , "origin" .= ("0" :: String)
       ]
      ]
    -- New polling endpoint for messages
    get "/api/messages" (messagesHandler stateVar)
    
    post "/api/general" (mainHandler stateVar)
    post "/api/test" postHandler
    post "/api/prac" (pracHandler stateVar)
    post "/api/practice" testHandler
    post "/api/newdata" newDataHandler
    post "/api/createuser" newUserHandler