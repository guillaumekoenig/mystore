{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.String.Conversions (cs)
import Data.Text (Text)
import Servant
import System.Directory (removeFile)

type StoreAPI =
       Capture "name" Text :> Get '[OctetStream] B.ByteString
  :<|> Capture "name" Text :> ReqBody '[OctetStream] B.ByteString
                           :> Put '[OctetStream] NoContent
  :<|> Capture "name" Text :> Delete '[OctetStream] NoContent

api :: Proxy StoreAPI
api = Proxy

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
