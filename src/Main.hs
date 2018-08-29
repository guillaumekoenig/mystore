module Main where

import Api (StoreAPI)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory (removeFile)

server :: Server StoreAPI
server = getFile :<|> putFile :<|> deleteFile

getFile :: Text -> Handler B.ByteString
getFile name = liftIO $ B.readFile (cs name)

putFile :: Text -> B.ByteString -> Handler NoContent
putFile name contents = do
  liftIO $ B.writeFile (cs name) contents
  return NoContent

deleteFile :: Text -> Handler NoContent
deleteFile name = do
  liftIO $ removeFile (cs name)
  return NoContent

app :: Application
app = serve (Proxy :: Proxy StoreAPI) server

main :: IO ()
main = run 8081 app
