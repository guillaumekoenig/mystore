{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (StoreAPI) where

import qualified Data.ByteString.Char8 as B
import Data.Text
import Servant

type StoreAPI =
       Capture "name" Text :> Get '[OctetStream] B.ByteString
  -- :<|> Capture "name" Text :> ReqBody '[OctetStream] B.ByteString
  --                          :> Put '[JSON] ()
  -- :<|> Capture "name" Text :> Delete '[JSON]
