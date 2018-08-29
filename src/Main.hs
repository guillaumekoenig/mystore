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
server = (liftIO . fileGet)
              -- :<|> (liftIO . filePut)
              :<|> (liftIO . fileDelete)

fileGet :: Text -> IO B.ByteString
fileGet name = B.readFile (cs name)

-- filePut :: Text -> B.ByteString -> IO ()
-- filePut name contents = B.writeFile (cs name) contents

fileDelete :: Text -> IO NoContent
fileDelete name = do
  removeFile (cs name)
  pure NoContent

app :: Application
app = serve (Proxy :: Proxy StoreAPI) server

main :: IO ()
main = run 8081 app
