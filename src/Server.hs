{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (server) where

import qualified Data.Aeson as A (FromJSON, ToJSON)
import qualified Data.ByteString as DB (ByteString)
import qualified Data.ByteString.Lazy as DBL
  ( ByteString,
    fromStrict,
    toStrict,
  )
import qualified Data.Text as DT (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
  )
import qualified Servant as S
  ( Accept (..),
    Application,
    Get,
    Handler,
    Header,
    JSON,
    OctetStream,
    Post,
    Proxy (Proxy),
    ReqBody,
    Server,
    serve,
  )
import Server.Custom (JsonAsOctetStream)

type Api = IncomingWebhookEndpoint :<|> OtherEndpoint

type IncomingWebhookEndpoint =
  "item" :> S.Header "x-signature" DT.Text :> S.ReqBody '[JsonAsOctetStream] DB.ByteString :> S.Post '[S.JSON] Response

type OtherEndpoint =
  "item" :> S.Get '[S.JSON] [Item]

incomingWebhookEndpoint :: S.Server IncomingWebhookEndpoint
incomingWebhookEndpoint = incomingWebhook

incomingWebhook :: Maybe DT.Text -> DB.ByteString -> S.Handler Response
incomingWebhook header body = do
  return $ Response "OK"

otherEndpoint :: S.Server OtherEndpoint
otherEndpoint = otherAction

otherAction :: S.Handler [Item]
otherAction = return [Item 123 "Test"]

endpoints :: S.Server Api
endpoints = incomingWebhookEndpoint :<|> otherEndpoint

api :: S.Proxy Api
api = S.Proxy

server :: IO S.Application
server = return $ S.serve api endpoints

data Response = Response {status :: DT.Text} deriving (Eq, Show, Generic)

instance A.ToJSON Response

instance A.FromJSON Response

data Item = Item
  { itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance A.ToJSON Item

instance A.FromJSON Item
