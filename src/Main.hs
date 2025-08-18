{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=), encode, eitherDecode, FromJSON, ToJSON)
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

data Incoming = Incoming
 { kind :: String
 , type_ :: String
 , message :: String
 , authcode :: String
 } deriving (Show, Generic)

instance FromJSON Incoming
instance ToJSON Incoming

generalJSON :: String -> String -> String -> String -> Value
generalJSON kind type_ message authcode =
 object
 [ "kind" .= kind
 , "data" .= object
 [ "type" .= type_
 , "message" .= message
 , "authcode" .= authcode
 ]
 ]

mainHandler :: ActionM ()
mainHandler = do
 bodyText <- body
 let decoded = eitherDecode bodyText :: Either String Incoming
 case decoded of
   Left err -> text $ TL.pack ("Invalid JSON: " ++ err)
   Right incoming -> do
     let jsonVal = generalJSON (kind incoming)
                              (type_ incoming)
                              (message incoming)
                              (authcode incoming)
     liftIO $ BL.putStrLn (encode jsonVal)
     text $ TL.fromStrict $ T.pack (BL.unpack (encode jsonVal))

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
     [ "message" .= ("Hello, World!" :: String)
     , "origin" .= ("0" :: String)
     ]
   post "/api/general" mainHandler
   post "/api/call" postHandler