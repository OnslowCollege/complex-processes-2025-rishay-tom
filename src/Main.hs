{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Control.Monad (forM_)
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
import Control.Monad.IO.Class (liftIO)

data IncomingData = IncomingData
  { dataMessage :: String
  , dataType :: String  
  , dataAuthcode :: String
  } deriving (Show, Generic)

data Incoming = Incoming
 { kind :: String
 , incomingData :: IncomingData
 } deriving (Show, Generic)

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

mainHandler :: ActionM ()
mainHandler = do
 bodyText <- body
 liftIO $ putStrLn $ "Raw body received: " ++ show (BL.take 200 bodyText)
 let decoded = eitherDecode bodyText :: Either String Incoming
 case decoded of
   Left err -> do
     liftIO $ putStrLn $ "JSON Parse Error: " ++ err
     liftIO $ putStrLn $ "Body length: " ++ show (BL.length bodyText)
     json $ object ["response" .= object ["error" .= ("Invalid JSON: " ++ err)]]
   Right incoming -> do
     let incomingDataObj = incomingData incoming
         jsonVal = generalJSON (kind incoming)
                              (dataType incomingDataObj)
                              (dataMessage incomingDataObj)
                              (dataAuthcode incomingDataObj)
         jsonBytes = encode jsonVal
         jsonString = BL.unpack jsonBytes
     liftIO $ putStrLn $ "Received: " ++ show incoming
     liftIO $ putStrLn $ "Responding with: " ++ jsonString
     json jsonVal

postHandler :: ActionM ()
postHandler = do
 bodyText <- body
 let modifiedStr = map toUpper (BL.unpack bodyText)
 text $ TL.pack ("Processed: " ++ modifiedStr)

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
 scotty 7879 $ do
   makeStaticHandlers "public"
   get "/api/test" $ text "Hello, World!"
   get "/api/message" $ json $ object
     [ "response" .= object
       [ "message" .= ("Hello, World!" :: String)
       , "origin" .= ("0" :: String)
       ]
     ]
   post "/api/general" mainHandler
   post "/api/call" postHandler