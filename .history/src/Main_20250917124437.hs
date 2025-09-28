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
  , users  :: [String]
  , messages  :: [String]
  , clients   :: [Client]
  , nextClientId :: ClientId
  , broadcastMessages :: [String] 
  } 

-- instance {-# OVERLAPPING #-} Show [Client] where
--   show clients = "Clients[" ++ show (length clients) ++ " connected]"


instance Show AppState where
  show (AppState uc users msgs clients ncid bcast) = 
    "AppState { userCount = " ++ show uc ++
    ", users = " ++ show users ++
    ", messages = " ++ show (length msgs) ++ " messages" ++
    ", clients = " ++ show (length clients) ++ " connected" ++
    ", nextClientId = " ++ show ncid ++
    ", broadcastMessages = " ++ show (length bcast) ++ " pending }"

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
-- A list of users, will use the DB

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


-- WebSocket Functions
addClient :: TVar AppState -> Connection -> STM ClientId
addClient stateVar conn = do
  state <- readTVar stateVar
  let clientId = nextClientId state
      newClient = (clientId, conn)
      newState = state 
        { clients = newClient : clients state
        , nextClientId = clientId + 1
        , userCount = userCount state + 1
        }
  writeTVar stateVar newState
  return clientId

removeClient :: TVar AppState -> ClientId -> STM ()
removeClient stateVar clientId = do
  state <- readTVar stateVar
  let newClients = filter ((/= clientId) . fst) (clients state)
      newState = state 
        { clients = newClients
        , userCount = length newClients
        }
  writeTVar stateVar newState

broadcastToClients :: TVar AppState -> ChannelMessage -> IO ()
broadcastToClients stateVar msg = do
  state <- readTVarIO stateVar
  let jsonMsg = encode msg
  forM_ (clients state) $ \(_, conn) -> do
    sendTextData conn jsonMsg

-- WebSocket application
websocketApp :: TVar AppState -> TChan ChannelMessage -> ServerApp
websocketApp stateVar broadcastChan pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30  -- Keep connection alive
  
  clientId <- atomically $ addClient stateVar conn
  putStrLn $ "Client " ++ show clientId ++ " connected"
  
  -- Notify other clients
  atomically $ writeTChan broadcastChan (UserJoined clientId)
  
  -- Handle client messages and cleanup
  finally 
    (handleClient stateVar broadcastChan clientId conn) 
    (do
      atomically $ do
        removeClient stateVar clientId
        writeTChan broadcastChan (UserLeft clientId)
      putStrLn $ "Client " ++ show clientId ++ " disconnected"
    )

handleClient :: TVar AppState -> TChan ChannelMessage -> ClientId -> Connection -> IO ()
handleClient stateVar broadcastChan clientId conn = forever $ do
  msgData <- receiveData conn
  case eitherDecode msgData of
    Left err -> putStrLn $ "WebSocket JSON error: " ++ err
    Right wsMsg -> do
      putStrLn $ "Received from client " ++ show clientId ++ ": " ++ show wsMsg
      case wsType wsMsg of
        "chat" -> do
          -- Add message to app state and broadcast
          atomically $ do
            state <- readTVar stateVar
            writeTVar stateVar state { messages = messages state ++ [wsMessage wsMsg] }
            writeTChan broadcastChan (UserMessage $ wsMessage wsMsg)
        "ping" -> sendTextData conn $ encode $ object ["type" .= ("pong" :: String)]
        _ -> putStrLn $ "Unknown message type: " ++ wsType wsMsg

-- Background task to handle channel messages
channelHandler :: TVar AppState -> TChan ChannelMessage -> IO ()
channelHandler stateVar broadcastChan = forever $ do
  msg <- atomically $ readTChan broadcastChan
  broadcastToClients stateVar msg

-- HTTP Polling compatibility functions
addBroadcastMessage :: TVar AppState -> String -> IO ()
addBroadcastMessage stateVar msg = do
  atomically $ do
    state <- readTVar stateVar
    writeTVar stateVar state { broadcastMessages = broadcastMessages state ++ [msg] }

getBroadcastMessages :: TVar AppState -> IO [String]
getBroadcastMessages stateVar = do
  result <- atomically $ do
    state <- readTVar stateVar
    writeTVar stateVar state { broadcastMessages = [] }
    return (broadcastMessages state)
  return result

-- Your existing handlers (modified to work with WebSockets and STM)
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

mainHandler :: TVar AppState -> TChan ChannelMessage -> ActionM ()
mainHandler stateVar broadcastChan = do
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

      -- Add message to state and broadcast via WebSocket
      liftIO $ atomically $ do
        state <- readTVar stateVar
        writeTVar stateVar state { messages = messages state ++ [dataMessage data_] }
        writeTChan broadcastChan (SystemMessage $ "API: " ++ dataMessage data_)

      -- Also add to HTTP polling messages for compatibility
      liftIO $ addBroadcastMessage stateVar ("API: " ++ dataMessage data_)

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

pracHandler :: TVar AppState -> TChan ChannelMessage -> ActionM ()
pracHandler stateVar broadcastChan = do
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

      liftIO $ atomically $ do
        state <- readTVar stateVar
        writeTVar stateVar state { messages = messages state ++ [dataMessage data_] }
        writeTChan broadcastChan (SystemMessage $ "Practice: " ++ dataMessage data_)

      liftIO $ addBroadcastMessage stateVar ("Practice: " ++ dataMessage data_)

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind data_)
      json jsonVal

messagesHandler :: TVar AppState -> ActionM ()
messagesHandler stateVar = do
  msgs <- liftIO $ getBroadcastMessages stateVar
  state <- liftIO $ readTVarIO stateVar
  json $ object 
    [ "messages" .= msgs
    , "userCount" .= userCount state
    , "allMessages" .= messages state
    , "connectedClients" .= length (clients state)
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

-- Static file handlers (unchanged)
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
  putStrLn "Starting server with WebSocket support on port 7879..."
  
  stateVar <- newTVarIO (AppState 0 [] [] [] 1 [])
  broadcastChan <- newBroadcastTChanIO
  
  _ <- async $ channelHandler stateVar broadcastChan
  
  scottyApp <- scottyApp $ do
    makeStaticHandlers "public"
    get "/api/test" $ text "Hello world api/test"
    get "/api/message" $ json $ object
      ["response" .= object
       [ "message" .= ("Hello, world" :: String)
       , "origin" .= ("0" :: String)
       ]
      ]
    get "/api/messages" (messagesHandler stateVar)
    get "/api/ws" (messagesHandler stateVar)  
    get "/api/users" $ json $ object
      ["users" .= ([] :: [String])
      ,"message" .= ("User list endpoint - TODO: implement database" :: String)
      ]
    
    post "/api/general" (mainHandler stateVar broadcastChan)
    post "/api/test" postHandler
    post "/api/prac" (pracHandler stateVar broadcastChan)
    post "/api/practice" testHandler
    post "/api/newdata" newDataHandler
    post "/api/createuser" newUserHandler

  putStrLn "Ready at http://localhost:7879"
  run 7879 $ websocketsOr 
    defaultConnectionOptions 
    (websocketApp stateVar broadcastChan) 
    scottyApp