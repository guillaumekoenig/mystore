module Main where

import Api (StoreAPI)
import qualified Data.ByteString.Char8 as B
import Network.Wai.Handler.Warp (run)
import Servant

server :: Server StoreAPI
server name = return $ B.pack "test"

app :: Application
app = serve (Proxy :: Proxy StoreAPI) server

main :: IO ()
main = run 8081 app
