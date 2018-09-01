{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api (api, server)
import Network.Wai.Handler.Warp (run)
import Options.Generic
import Servant (serve)
import System.Directory (setCurrentDirectory)

data Config w = Config
  { port :: w ::: Int <?> "Port to serve requests on"
  , folder :: w ::: String <?> "Folder to serve"
  } deriving (Generic)

instance ParseRecord (Config Wrapped)
deriving instance Show (Config Unwrapped)

main :: IO ()
main = do
  config <- unwrapRecord "Mystore"
  setCurrentDirectory (folder config)
  putStrLn $ "Serving \"" ++ folder config
    ++ "\" on :" ++ show (port config)
  run (port config) (serve api server)
