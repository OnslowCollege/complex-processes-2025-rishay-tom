{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import qualified Zero.Server as Server
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (forM)
import System.FilePath (takeFileName)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List (isSuffixOf)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toUpper)

mainHandler :: Server.Handler
mainHandler =
  Server.simpleHandler Server.GET "/hello" (\_ -> Server.stringResponse "HI!!!")

--string
testHandler :: Server.Handler
testHandler = 
  Server.simpleHandler Server.GET "/api/test" (\_ -> Server.stringResponse "Hello, World!")

--json
test2Handler :: Server.Handler
test2Handler =
  Server.simpleHandler Server.GET "/api/message" $ \_ ->
    Server.jsonResponse (object
    [ "message" .= ("Hello, World!" :: String)
    , "origin"  .= ("0" :: String)
    ])

--post
postHandler :: Server.Handler
postHandler = Server.simpleHandler Server.POST "/api/call" myPostHandler

myPostHandler :: Server.Request -> Server.Response
myPostHandler req = Server.stringResponse result
  where
    body = Server.requestBody req
    modifiedStr = map toUpper body
    result = "Processed: " ++ modifiedStr

-- generalHander :: Server.Handler 
-- generalHander = S

--practiceHandler :: Server.Handler practiceHandler = Server.simpleHandlerServer

--'tick' 
-- /api/message''


-- /api/nodes''
-- The list of nodes avalible to change, there is node system, but we will return one or zero for compatability reasons

-- /api/buttons''
-- A list of buttons, and links with the buttons in a specific json format, will connect to the DB

-- /api/servers''
-- A list of servers, you should be able to switch servers (have multiple coccurrent servers running), uses db

-- /api/users''
-- A list of users, will use the db

-- /api/ws''
-- A websocket (ws)
-- Needs a specific way of setting up the endpoint, and then client subscribes to it
-- Logs from/to the server will be sent to it

-- /api/awaitserverstatus''
-- A server side event (SSE)
-- Needs a specific way of setting up the endpoint, and then client subscribes to it
-- it will continutally share the status of the server, which is down or up (each message is either down or up)


-- /api/getstatus''
-- Gets a status of one or more things, is a post request, is returned with the status of that thing, like
-- if the buttons are toggled to defaults or not


-- /api/getfiles''
-- returns the files from the server location, will not consult the db, rather list the directory contents and navigate accordingly


-- /api/buttonreset''
-- Will either toggle to defaults or reset to defaults, the latter is irreversable, post request and the message/command determines which

-- /api/editbuttons''
-- post request with a new buttons link, takes name, link, and all relevent feilds

-- /api/addnode''
-- post request with a nodes information, creates the node in db, since we are not dealing with nodes, we will have the handler
-- but omit the db adding logic and just return the relevent statuscode

-- /api/edituser''
-- post request to edit the user with the new information, will consult the db

-- /api/getuser''
-- retrives a full user based on the users name, returns in json

-- /api/send''*impo
-- sends a message originally to the tcp server, we are not having that since all server running logic will 
-- be done entirely or primarially here, and not delegate the actual creation to another sub-project

-- /api/general''*impo
-- For general messages, in alot if not most cases this is for development purposes
-- there are many cases where this can fail, if it does, it can simply return INTERNAL_SERVER_ERROR
-- it forwards the messages to the channel which forwards it to the gameserver

-- /api/signin''
-- The sign in function which is the main part of authentication
-- rely on the database to try and find the user entry, if it fails, its immediately unauthorized, or it will try and match the password next
-- if it fails, its unauthorized
-- post request, takes login data as json, NOT A JWT, returns jwt

-- /api/createuser''
-- information to create a user, will consult the db, is a post request

-- /api/deleteuser''
-- information to delete a user, will consult the db, is a post request

-- /authenticate
-- This is crucial for authentication, it will take a next for redirects, and a jwk to verify the claim with, then it grants the claim for the current session
-- and redirects the user to their original destination
-- will be used after sign in, which returns a jwt

makeHtmlHandler :: FilePath -> IO Server.Handler
makeHtmlHandler filepath = do
  content <- TIO.readFile filepath
  let replaced = T.replace (T.pack "[[SITE_URL]]") (T.pack "/static") content
      url = "/static/" ++ (takeFileName filepath) 
  return $ Server.simpleHandler Server.GET url (\_ -> Server.stringResponse (T.unpack replaced))

makeStaticHandlers :: FilePath -> IO [Server.Handler]
makeStaticHandlers dir = do
    files <- listDirectory dir
    handlers <- forM files $ \fname -> do
        let path = dir </> fname
        isFile <- doesFileExist path
        if isFile
          then if ".html" `isSuffixOf` fname
            then makeHtmlHandler path
            else do
              content <- readFile path
              let url = "/static/" ++ fname
              return $ Server.simpleHandler Server.GET url (\_ -> Server.stringResponse content)
          else return $ Server.simpleHandler Server.GET "" (\_ -> Server.stringResponse "")
    return handlers


main :: IO ()
main = do
  putStrLn "[LOG] Starting minecraft-dashboard application..."
  Lib.run
  putStrLn "[LOG] Finished running Lib.run"

  staticHandlers <- makeStaticHandlers "public"
  Server.startServer (staticHandlers ++ [test2Handler, testHandler, mainHandler, postHandler])