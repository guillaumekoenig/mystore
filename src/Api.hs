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

-- | Type-level representation of the API.
--
-- All three routes capture a name from URL (name of
-- the file), with each route being one of GET, PUT,
-- DELETE http verbs to manipulate file. PUT requests
-- additionally have a request body (contents of the
-- file), and GET requests respond contents of file.
type StoreAPI =
       Capture "name" Text :> Get '[OctetStream] B.ByteString
  :<|> Capture "name" Text :> ReqBody '[OctetStream] B.ByteString
                           :> Put '[OctetStream] NoContent
  :<|> Capture "name" Text :> Delete '[OctetStream] NoContent

-- | Proxy to have an actual value for our API.
api :: Proxy StoreAPI
api = Proxy

-- | Top-level definition of the server: it binds
-- together the implementation of all three routes.
server :: Server StoreAPI
server = getFile :<|> putFile :<|> deleteFile

-- | Retrieve a file based on name, contents are
-- returned in the http response.
getFile :: Text -> Handler B.ByteString
getFile name = mapFsErr $ B.readFile (cs name)

-- | Update or create a file with name, contents
-- are passed in the request body.
putFile :: Text -> B.ByteString -> Handler NoContent
putFile name contents = mapFsErr $ do
  B.writeFile (cs name) contents
  return NoContent

-- | Delete file given by name.
deleteFile :: Text -> Handler NoContent
deleteFile name = mapFsErr $ do
  removeFile (cs name)
  return NoContent

-- | Map filesystem errors to http errors.
-- In particular, we return 404 when dealing with
-- a non existent file (on get, delete), and 500
-- for other kinds of filesystem errors (eg disk
-- full).
mapFsErr :: IO a -> Handler a
mapFsErr ioaction = do
  r <- liftIO $ try $ ioaction
  case r of
    Left (e@IOError { ioe_errno = Just 2 }) -- No such file or directory
      -> Handler (throwE $ err404 {errBody = cs $ ioe_description e})
    Left e                      -- Other filesystem errors
      -> Handler (throwE $ err500 {errBody = cs $ ioe_description e})
    Right val -> return val
