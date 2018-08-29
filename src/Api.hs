{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (StoreAPI) where

import qualified Data.ByteString.Char8 as B
import Data.Text
import Servant

type StoreAPI =
       Capture "name" Text :> Get '[OctetStream] B.ByteString
  :<|> Capture "name" Text :> ReqBody '[OctetStream] B.ByteString
                           :> Put '[OctetStream] NoContent
  :<|> Capture "name" Text :> Delete '[OctetStream] NoContent
