{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (api, server)
import Network.Wai.Handler.Warp (run)
import Options.Generic
import Servant (serve)
import System.Directory (setCurrentDirectory)

data Config = Config
  { port :: Int
  , folder :: String
  } deriving (Generic)

instance ParseRecord Config

main :: IO ()
main = do
  config <- getRecord "Mystore"
  setCurrentDirectory (folder config)
  putStrLn $ "Serving \"" ++ folder config
    ++ "\" on :" ++ show (port config)
  run (port config) (serve api server)
