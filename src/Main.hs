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

mainHandler :: Server.Handler
mainHandler =
  Server.simpleHandler Server.GET "/hello" (\_ -> Server.stringResponse "HI!!!")

testHandler :: Server.Handler
testHandler =
  Server.simpleHandler Server.GET "/test" $ \_ ->
    Server.jsonResponse (object ["message" .= ("HI!!!" :: String)])
-- generalHander :: Server.Handler 
-- generalHander = S

practiceHandler :: Server.Handler practiceHandler = Server.simpleHandlerServer

--'tick' 
-- /api/message''
-- /api/nodes''
-- /api/buttons''
-- /api/servers''
-- /api/users''
-- /api/ws''
-- /api/awaitserverstatus''
-- /api/getstatus''
-- /api/getfiles''
-- /api/buttonreset''
-- /api/editbuttons''
-- /api/addnode''
-- /api/edituser''
-- /api/getuser''
-- /api/send''*impo
-- /api/general''*impo
-- /api/signin''
-- /api/createuser''
-- /api/deleteuser''



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
  Server.startServer (mainHandler : staticHandlers ++ [testHandler])

