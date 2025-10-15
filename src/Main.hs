{-# LANGUAGE OverloadedStrings #-}
-- This will allow me to use/convert string types
-- so like strings can be used as bytestrings and etc
{-# LANGUAGE DeriveGeneric #-}
-- This means it will generate instances for peices of data
-- this is useful for seralizing and so on
{-# LANGUAGE InstanceSigs #-}
-- When declaring instances, you can declare a coorosponding type
{-# LANGUAGE DuplicateRecordFields #-}
-- This will allow me to have multiple data types with the same feild name
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Control.Monad (forM_, forever, void, when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Data.Aeson (Value, object, (.=), encode, eitherDecode, FromJSON, ToJSON, parseJSON, toJSON, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
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
import qualified Control.Exception as E
import Control.Exception (finally, SomeException)
import Network.HTTP.Types (status405)
import Data.IORef
import System.Process
import System.IO (Handle, hPutStrLn, hGetLine, hClose, hIsEOF, hSetBuffering, BufferMode(..))
import GHC.IO.Handle (hGetContents)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))

import System.Info (os)

data ProcessConfig = ProcessConfig
  { prehookCmds  :: [String]
  , installCmds  :: [String]
  , posthookCmds :: [String]
  , runCmd       :: String
  } deriving (Show)

-- This is the commands that it will run when the create server api route is called
-- I have two sets of commands, one for linux and one for windows
defaultProcessConfig :: ProcessConfig
defaultProcessConfig =
  case os of
    "mingw32" ->  -- For windows systems like toms
      ProcessConfig
        { prehookCmds =
            [ "mkdir server" ]
        , installCmds =
            [ "cd server && curl -o server.jar https://piston-data.mojang.com/v1/objects/84194a2f286ef7c14ed7ce0090dba59902951553/server.jar"
            , "cd server && echo eula=true > eula.txt"
            ]
        , posthookCmds =
            [ "echo Server setup complete" ]
        , runCmd = "cd server && java -Xmx2G -Xms1G -jar server.jar nogui"
        }

    _ ->  -- For linux systems like mine
      ProcessConfig
        { prehookCmds =
            [ "mkdir -p server" ]
        , installCmds =
            [ "cd server && wget -O server.jar https://piston-data.mojang.com/v1/objects/84194a2f286ef7c14ed7ce0090dba59902951553/server.jar"
            , "cd server && echo 'eula=true' > eula.txt"
            ]
        , posthookCmds =
            [ "echo Server setup complete" ]
        , runCmd = "cd server && java -Xmx2G -Xms1G -jar server.jar nogui"
        }

data GeneralDBType = GeneralDBType
  { users :: [String]
  } deriving (Show, Generic)

-- AppState for data that needs to be accessed across all of the routes
data AppState = AppState
  { userCount :: Int
  , messages  :: [String]
  , wsOutgoing :: TBQueue T.Text
  , wsIncoming :: TBQueue T.Text
  , processHandle :: Maybe (Handle, Handle, Handle, ProcessHandle)  -- stdin, stdout, stderr, process
  , generalDB :: GeneralDBType
  }

-- TODO: remove this (or rename to something better)
data NewDataType = NewDataType
  { message :: String
  , effect :: String
  } deriving (Show, Generic)  

-- TODO: used for the test handler, remove at some point
data ModifiedJson = ModifiedJson 
  { onemessage :: String
  , status :: String
  } deriving (Show, Generic)

data UserList = UserList
  { userlist :: [String]
  } deriving (Show, Generic)

-- This is the data type used for the majority of routes
-- uses a message, type and authcode,
-- I do not intend to implimentent comprehensive auth, but this is for backwards compatibility as 
-- I copied it from my other project, message type and message just helps catagorize it,
-- e.g a message type of console has its message forwarded to the console and command would run some basic server commands
data IncomingData = IncomingData
  { dataMessage :: String
  , dataType :: String  
  , dataAuthcode :: String
  } deriving (Show, Generic)

-- TODO: Consider removing this, this is used as a part of some other data types but
-- is not of much significance
data Incoming = Incoming
  { kind :: String
  , incomingData :: IncomingData
  } deriving (Show, Generic)

-- IncomingUser is just the general user data, with the password (plaintext not hashed), its
-- perms which will be barely or not used but kept for backwards compatibility and its user (for username)
data IncomingUser = IncomingUser
  { user :: String
  , password :: String
  , user_perms :: [String]
  } deriving (Show, Generic)

-- TODO: Make element more generic, it is not just a User but could sometimes be other stuff
-- Element is just here for backwards compatibility, in the other language i tried this on,
-- i used enums which required a kind feild, so I kept it here as well as just using this for nesting
data Element = Element
  { kind :: String
  , incomingUser :: IncomingUser
  } deriving (Show, Generic)

-- Wrapped is a reusable pattern containing a kind and data feild, these feilds are quite common so 
-- abstracting it into 'Wrapped' makes sense, it wraps around the data
data Wrapped a = Wrapped
  { kind :: String
  , data_ :: a
  } deriving (Show, Generic)

-- Envelope is another wrapping type, to wrap around data, usually Envelop only goes around Wrapped, because
-- p
data Envelope a = Envelope
  { element :: Wrapped a
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Envelope a) where
  parseJSON = withObject "Envelope" $ \o ->
    Envelope <$> o .: "element"

instance FromJSON UserList where
  parseJSON = withObject "UserList" $ \o -> UserList
    <$> o .: "users"

instance ToJSON UserList where
  toJSON (UserList m) = object
    [ "users" .= m
    ] 

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

-- Execute a list of commands sequentially
executeCommands :: [String] -> IO ()
executeCommands cmds = forM_ cmds $ \cmd -> do
  putStrLn $ "Executing: " ++ cmd
  (_, _, _, ph) <- createProcess (shell cmd)
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> putStrLn $ "Command succeeded: " ++ cmd
    ExitFailure code -> putStrLn $ "Command failed with code " ++ show code ++ ": " ++ cmd
  return ()

-- Initialize the process with prehook, install, posthook, then start the run command
initializeProcess :: ProcessConfig -> IO (Handle, Handle, Handle, ProcessHandle)
initializeProcess config = do
  -- this runs functions which executes the command based on the config
  executeCommands (prehookCmds config)
  executeCommands (installCmds config)
  executeCommands (posthookCmds config)
  

  putStrLn $ "Starting main process: " ++ runCmd config
  (Just hin, Just hout, Just herr, ph) <- createProcess (shell $ runCmd config)
    {
    -- Create pipes for all 3 forms of back and forth communication, its useful because otherwise there would be no control 
    -- over the program and output, like a channel, but not nessesairly for multi-threaded purposes
    std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  
    -- This ensures that we are given items from stdin, stout, and stderr when a new line comes it
    -- and not just a character as it will send alot of information to the 
  hSetBuffering hin LineBuffering
  hSetBuffering hout LineBuffering
  hSetBuffering herr LineBuffering
  
  return (hin, hout, herr, ph)

-- this will spawn a thread and check for timeouts, then terminates the thread, 
-- the thread is the thing that contains the process
processOutputReader :: IORef AppState -> Handle -> IO ()
processOutputReader stateRef hout = forever $ do
  eof <- hIsEOF hout
  if eof
    then do
      putStrLn "Process stdout closed"
      threadDelay 1000000
    else do
      line <- TIO.hGetLine hout
      putStrLn $ "Process output: " ++ T.unpack line
      state <- readIORef stateRef
      atomically $ writeTBQueue (wsOutgoing state) line
  -- if there is an error, this will catch it in terms of reading the output
  `E.catch` \(e :: SomeException) -> do
    putStrLn $ "Error reading process output: " ++ show e
    threadDelay 1000000

--  This is another thread that will process errors specifically, as all stdin, stoudout and stderr should be handled seperately
processErrorReader :: IORef AppState -> Handle -> IO ()
processErrorReader stateRef herr = forever $ do
  eof <- hIsEOF herr
  if eof
    then do
      putStrLn "Process stderr closed"
      threadDelay 1000000
    else do
      line <- TIO.hGetLine herr
      putStrLn $ "Process error: " ++ T.unpack line
      state <- readIORef stateRef
      atomically $ writeTBQueue (wsOutgoing state) ("ERROR: " <> line)
  `E.catch` \(e :: SomeException) -> do
    putStrLn $ "Error reading process errors: " ++ show e
    threadDelay 1000000

-- this is another thread which will handle the input writer (stdin) seprately
processInputWriter :: IORef AppState -> Handle -> IO ()
processInputWriter stateRef hin = forever $ do
  -- gets the state
  state <- readIORef stateRef
  -- gets the msg from the queue
  msg <- atomically $ readTBQueue (wsIncoming state)
  -- output it
  TIO.hPutStrLn hin msg
  putStrLn $ "Sent to process: " ++ T.unpack msg
  -- if there is a error writing to the process (sending the command to the process)
  -- send it to it
  `E.catch` \(e :: SomeException) -> do
    putStrLn $ "Error writing to process: " ++ show e
    threadDelay 1000000

startServerProcess :: IORef AppState -> IO ()
startServerProcess stateRef = do
  maybeProcess <- processHandle <$> readIORef stateRef
  case maybeProcess of
    Just _ -> putStrLn "Process already running"
    Nothing -> do
      putStrLn "Starting server process..."
      (hin, hout, herr, ph) <- initializeProcess defaultProcessConfig
      modifyIORef stateRef $ \s -> s { processHandle = Just (hin, hout, herr, ph) }
      
      _ <- forkIO $ processOutputReader stateRef hout
      _ <- forkIO $ processErrorReader stateRef herr
      _ <- forkIO $ processInputWriter stateRef hin
      
      putStrLn "Process started and I/O threads spawned"

-- This starts the websocket, it takes appstate, and the incoming websocket connection waiting to be accepted
-- IO returns nothing like with all other handlers (signifies it can preform IO operations inside)
wsApp :: IORef AppState -> PendingConnection -> IO ()
wsApp stateRef pending = do
  conn <- acceptRequest pending
  putStrLn "New WebSocket connection established"
  
  sendTextData conn ("Welcome to WebSocket server!" :: T.Text)
  
  state <- readIORef stateRef

  -- Forward messages from process to WebSocket client
  _ <- forkIO $ forever $ do
    msg <- atomically $ readTBQueue (wsOutgoing state)
    sendTextData conn msg
    putStrLn $ "Sent to WebSocket: " ++ T.unpack msg
  
  -- Forward messages from WebSocket client to process
  flip finally (putStrLn "WebSocket connection closed") $ forever $ do
    msg <- receiveData conn
    putStrLn $ "Received from WebSocket: " ++ T.unpack msg
    atomically $ writeTBQueue (wsIncoming state) msg


-- Helper function to send message to WebSocket clients
-- this has helped me in testing
-- remove in final product
sendToWebSocket :: IORef AppState -> T.Text -> IO ()
sendToWebSocket stateRef msg = do
  state <- readIORef stateRef
  atomically $ writeTBQueue (wsOutgoing state) msg

-- Help function to read messages from WebSocket clients
-- this has helped me in testing
-- remove in final product
readFromWebSocket :: IORef AppState -> IO (Maybe T.Text)
readFromWebSocket stateRef = do
  state <- readIORef stateRef
  atomically $ do
    empty <- isEmptyTBQueue (wsIncoming state)
    if empty
      then return Nothing
      else Just <$> readTBQueue (wsIncoming state)

-- general handlder for WS, discriminate between POST and GET processes I guess
-- although I usually use POST
-- Useful for initialization of the websocket
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

-- TODO: remove this later.
-- Like prac handler, this was kept around for testing purposes, for more simpler testing
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

-- TODO: remove this later
-- This is used for testing stuff like a mutable app state 
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

userHandler :: IORef AppState -> ActionM ()
userHandler stateRef = do
  state <- liftIO $ readIORef stateRef
  let allUsers = users (generalDB state)
  let jsonVal = UserList allUsers
  json jsonVal



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

      let username = user userData
      liftIO $ modifyIORef stateRef $ \s ->
        let oldDB = generalDB s
            -- Only add if user doesn't already exist
            newUsers = if username `elem` users oldDB
                      then users oldDB  -- User exists, don't add
                      else users oldDB ++ [username]  -- User doesn't exist, add
            newDB = oldDB { users = newUsers }
        in s { generalDB = newDB }

      liftIO $ putStrLn $ "Received: " ++ show (Wrapped kind userData)
      liftIO $ putStrLn $ "Responding with: " ++ jsonString
      json jsonVal
-- reference  
-- body: JSON.stringify({
--     element: { 
--         kind: "User", 
--         data: { 
--             user, password, user_perms
--         }
--     },
--     require_auth: true,
--     jwt
-- })

    
-- user handler, modify in appstate

-- Mime types signify what purpose something has when presented in the browser, which in turn
-- tells it how to display it, previously the html and other files were being sent with the wrong mime type, which lead it to 
-- for example show html as its code rather than post rendered
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

-- Deletes the user
deleteUserHandler :: IORef AppState -> ActionM ()
deleteUserHandler stateRef = do
  bodyText <- body
  let decoded = eitherDecode bodyText :: Either String (Envelope IncomingUser)
  case decoded of 
    Left err -> do 
      -- generaldb 
      liftIO $ putStrLn $ "JSON Parse Error: " ++ err
      liftIO $ putStrLn $ "Body length: " ++ show (BL.length bodyText)
      json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]

    Right (Envelope (Wrapped kind userData)) -> do
      let username = user userData
      
      -- Get current state to check
      currentState <- liftIO $ readIORef stateRef
      let currentUsers = users (generalDB currentState)
      
      if username `elem` currentUsers
        then do
          -- User exists, delete them
          liftIO $ modifyIORef stateRef $ \s ->
            --derive generaldb from stateref 's'
            let oldDB = generalDB s
            -- make new  without targeg user
                newUsers = filter (/= username) (users oldDB)
                newDB = oldDB { users = newUsers }
            in s { generalDB = newDB
                }
          
          json $ object 
            [ "status" .= ("success" :: String)
            , "message" .= ("User deleted: " ++ username)
            ]
        else do
          -- User doesn't exist
          json $ object
            [ "status" .= ("error" :: String)
            , "message" .= ("User not found: " ++ username)
            ]


--
     


-- data AppState = AppState
--   { userCount :: Int
--   , messages  :: [String]
--   , wsOutgoing :: TBQueue T.Text
--   , wsIncoming :: TBQueue T.Text
--   , processHandle :: Maybe (Handle, Handle, Handle, ProcessHandle)  -- stdin, stdout, stderr, process
--   , generalDB :: GeneralDBType
--   }

--Structure            body: JSON.stringify({ element: user, jwt })

-- Main code that replaces its internal url path, so that it can call the files using the given subdirectory
makeHtmlHandler :: FilePath -> ActionM ()
makeHtmlHandler filepath = do
 content <- liftIO $ TLIO.readFile filepath
 let replaced = TL.replace "[[SITE_URL]]" "/" content
 html replaced

-- 
makeStaticHandlers :: FilePath -> ScottyM ()
makeStaticHandlers dir = do
 get "/:filename" $ do
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

--calls/links handlers with their functions
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
  get "/api/users" (userHandler stateRef)
  post "/api/general" mainHandler
  post "/api/test"  postHandler
  post "/api/prac" (pracHandler stateRef)
  post "/api/practice" testHandler
  post "/api/newdata" newDataHandler
  post "/api/createuser" (newUserHandler stateRef)
  post "/api/deleteuser" (deleteUserHandler stateRef)
  
  get "/api/ws" (wsHandler stateRef)
  post "/api/ws" (wsHandler stateRef)

main :: IO ()
main = do
  putStrLn "Server starting on port 7879..."
  
  outgoingQueue <- newTBQueueIO 100
  incomingQueue <- newTBQueueIO 100
  
  stateRef <- newIORef (AppState 
    { userCount = 0
    , messages = []
    , wsOutgoing = outgoingQueue
    , wsIncoming = incomingQueue
    , processHandle = Nothing
    , generalDB = GeneralDBType { users = [] } 
    })

  startServerProcess stateRef

  scottyApp <- scottyApp $ createScottyApp stateRef
  
  putStrLn "WebSocket and HTTP server running on port 7879..."
  putStrLn "WebSocket endpoint: ws://localhost:7879/ws"
  putStrLn "HTTP endpoints available at: http://localhost:7879/"
  
  run 7879 $ websocketsOr defaultConnectionOptions (wsApp stateRef) scottyApp