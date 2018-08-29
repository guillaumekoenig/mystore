module Main where

import Api (StoreAPI)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.String.Conversions (cs)
import Network.Wai.Handler.Warp (run)
import Servant

server :: Server StoreAPI
server name = liftIO $ B.readFile (cs name)

app :: Application
app = serve (Proxy :: Proxy StoreAPI) server

main :: IO ()
main = run 8081 app
