module Main where

import Api (api, server)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

main :: IO ()
main = run 8081 (serve api server)
