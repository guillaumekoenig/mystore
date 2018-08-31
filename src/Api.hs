{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api, server) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString.Char8 as B
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.IO.Exception (IOException(..))
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
getFile name = mapFsErr $ B.readFile (cs name)

putFile :: Text -> B.ByteString -> Handler NoContent
putFile name contents = mapFsErr $ do
  B.writeFile (cs name) contents
  return NoContent

deleteFile :: Text -> Handler NoContent
deleteFile name = mapFsErr $ do
  removeFile (cs name)
  return NoContent

mapFsErr :: IO a -> Handler a
mapFsErr ioaction = do
  r <- liftIO $ try $ ioaction
  case r of
    Left (e@IOError { ioe_errno = Just 2 }) -- No such file or directory
      -> Handler (throwE $ err404 {errBody = cs $ ioe_description e})
    Left e                      -- Other filesystem errors
      -> Handler (throwE $ err500 {errBody = cs $ ioe_description e})
    Right val -> return val
