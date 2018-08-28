{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.ByteString (ByteString)
import Data.Text
import Servant

type StoreAPI =
       Capture "name" Text :> Get
  :<|> Capture "name" Text :> ReqBody '[OctetStream] ByteString
  :<|> Capture "name" Text :> Delete

type Name = Text
