{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (server, AppCtx) where

import qualified Data.Aeson as A (FromJSON, ToJSON(toEncoding), encode, genericToEncoding, defaultOptions)
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
    (:>), hoistServerWithContext, hoistServer,
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
    ServerT,
    Context(EmptyContext),
    serve,
    serveWithContext,
    serveWithContextT,
  )
import Server.Custom (JsonAsOctetStream)
import qualified System.Log.FastLogger as FL (LoggerSet, ToLogStr(toLogStr), pushLogStrLn)
import qualified Data.Time as DTi (UTCTime, getCurrentTime)
import qualified Control.Monad.Trans.Reader as TR (ReaderT(runReaderT), asks)
import Control.Monad.IO.Class (liftIO)

type AppM = TR.ReaderT AppCtx S.Handler

newtype AppCtx = AppCtx {_getLogger :: FL.LoggerSet}

type Api = IncomingWebhookEndpoint :<|> OtherEndpoint

type IncomingWebhookEndpoint =
  "item" :> S.Header "x-signature" DT.Text :> S.ReqBody '[JsonAsOctetStream] DB.ByteString :> S.Post '[S.JSON] Response

type OtherEndpoint =
  "item" :> S.Get '[S.JSON] [Item]

incomingWebhookEndpoint :: S.ServerT IncomingWebhookEndpoint AppM
incomingWebhookEndpoint = incomingWebhook

incomingWebhook :: Maybe DT.Text -> DB.ByteString -> AppM Response
incomingWebhook header body =
  do
    logset <- TR.asks _getLogger
    tstamp <- liftIO DTi.getCurrentTime
    let logMsg =
          LogMessage
            { message = "let's do some logging!",
              timestamp = tstamp,
              level = "info"
            }
    -- emit log message
    liftIO $ FL.pushLogStrLn logset $ FL.toLogStr logMsg
    return $ Response "OK"

data LogMessage = LogMessage
  { message :: !DT.Text,
    timestamp :: !DTi.UTCTime,
    level :: !DT.Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON LogMessage

instance A.ToJSON LogMessage where
  toEncoding = A.genericToEncoding A.defaultOptions

instance FL.ToLogStr LogMessage where
  toLogStr = FL.toLogStr . A.encode

otherEndpoint :: S.ServerT OtherEndpoint AppM
otherEndpoint = otherAction

otherAction :: AppM [Item]
otherAction = return [Item 123 "Test"]

endpoints :: S.ServerT Api AppM
endpoints = incomingWebhookEndpoint :<|> otherEndpoint

api :: S.Proxy Api
api = S.Proxy

server :: AppCtx -> IO S.Application
server ctx = return $ S.serveWithContext api S.EmptyContext $ (flip TR.runReaderT ctx) endpoints

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
