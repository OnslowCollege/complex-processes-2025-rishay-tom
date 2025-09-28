{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where
import Control.Monad (forM_, forever, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
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
import Network.WebSockets (Connection, receiveData, sendTextData, withPingThread, PendingConnection, acceptRequest)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (requestMethod)
import Control.Exception (finally)
import Network.HTTP.Types (status405)
import Data.IORef

-- Updated AppState with STM channels for WebSocket communication
data AppState = AppState
  { userCount :: Int
  , messages  :: [String]
  , wsOutgoing :: TBQueue T.Text  -- Messages to send to WebSocket clients
  , wsIncoming :: TBQueue T.Text  -- Messages received from WebSocket clients
  }

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

--type
data Envelope a = Envelope
  { element :: Wrapped a
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Envelope a) where
  parseJSON = withObject "Envelope" $ \o ->
    Envelope <$> o .: "element"

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
  toJSON :: Element -> Value
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

dataBaseJSON :: String -> String -> String -> String -> Value
dataBaseJSON user settings nodes servers =
 object
 [ "user" .= user
 , "settings" .= settings
 , "nodes" .= nodes
 , "servers" .= servers
 ]

wsApp :: IORef AppState -> PendingConnection -> IO ()
wsApp stateRef pending = do
  conn <- acceptRequest pending
  putStrLn "New WebSocket connection established"
  
  sendTextData conn ("Welcome to WebSocket server!" :: T.Text)
  
  state <- readIORef stateRef

  _ <- forkIO $ forever $ do
    msg <- atomically $ readTBQueue (wsOutgoing state)
    sendTextData conn msg
    putStrLn $ "Sent to WebSocket: " ++ T.unpack msg
  
  flip finally (putStrLn "WebSocket connection closed") $ forever $ do
    msg <- receiveData conn
    putStrLn $ "Received from WebSocket: " ++ T.unpack msg
    atomically $ writeTBQueue (wsIncoming state) msg
    
    sendTextData conn $ "Echo: " <> msg

-- Helper function to send message to WebSocket clients
sendToWebSocket :: IORef AppState -> T.Text -> IO ()
sendToWebSocket stateRef msg = do
  state <- readIORef stateRef
  atomically $ writeTBQueue (wsOutgoing state) msg

-- Helper function to read messages from WebSocket clients
readFromWebSocket :: IORef AppState -> IO (Maybe T.Text)
readFromWebSocket stateRef = do
  state <- readIORef stateRef
  atomically $ do
    empty <- isEmptyTBQueue (wsIncoming state)
    if empty
      then return Nothing
      else Just <$> readTBQueue (wsIncoming state)

wsHandler :: IORef AppState -> ActionM ()
wsHandler stateRef = do
  method <- request >>= return . requestMethod
  case method of
    "POST" -> do
      bodyText <- body
      let message = T.pack $ BL.unpack bodyText
      liftIO $ sendToWebSocket stateRef message
      json $ object ["status" .= ("Message sent to WebSocket" :: String)]
    
    "GET" -> do
      maybeMsg <- liftIO $ readFromWebSocket stateRef
      case maybeMsg of
        Nothing -> json $ object 
          [ "status" .= ("no messages" :: String)
          , "message" .= ("" :: String)
          ]
        Just msg -> json $ object 
          [ "status" .= ("message received" :: String)
          , "message" .= msg
          ]
    
    _ -> do
      Web.Scotty.status status405
      json $ object ["error" .= ("Method not allowed" :: String)]

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

newUserHandler :: IORef AppState -> ActionM ()
newUserHandler stateRef = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String (Envelope IncomingUser)

  case decoded of
    Left err -> do
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      liftIO $ putStrLn $ "Body length: " ++ show (BL.length bodyText)
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Envelope (Wrapped kind userData)) -> do
      let jsonVal = dataBaseJSON (user userData) "" "" ""
          jsonBytes = encode jsonVal
          jsonString = BL.unpack jsonBytes

      liftIO $ modifyIORef stateRef $ \s ->
        s { userCount = userCount s + 1
          , messages = messages s ++ [user userData]
          }

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind userData)
      liftIO $ putStrLn $ "Responding with: " ++ jsonString
      json jsonVal
    
-- user handler, modify in appstate

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

createScottyApp :: IORef AppState -> ScottyM ()
createScottyApp stateRef = do
  makeStaticHandlers "public"
  get "/api/test" $ text "Hello world api/test"
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
  post "/api/createuser" (newUserHandler stateRef)
  
  get "/api/ws" (wsHandler stateRef)
  post "/api/ws" (wsHandler stateRef)

main :: IO ()
main = do
  putStrLn "Server starting on port 7879..."
  
  outgoingQueue <- newTBQueueIO 100
  incomingQueue <- newTBQueueIO 100
  
  stateRef <- newIORef (AppState 0 [] outgoingQueue incomingQueue)
  
  scottyApp <- scottyApp $ createScottyApp stateRef
  
  putStrLn "WebSocket and HTTP server running on port 7879..."
  putStrLn "WebSocket endpoint: ws://localhost:7879/ws"
  putStrLn "HTTP endpoints available at: http://localhost:7879/"
  
  run 7879 $ websocketsOr defaultConnectionOptions (wsApp stateRef) scottyApp