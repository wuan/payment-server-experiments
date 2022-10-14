{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( run,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Coerce (coerce)
import qualified Data.Text as DT (Text, pack, unpack)
import qualified Data.Text.Lazy as DTL (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp (defaultSettings, runSettings, setBeforeMainLoop, setLogger, setPort)
import qualified Network.Wai.Logger as WL (withStdoutLogger)
import Server (server, AppCtx)
import System.IO (hPutStrLn, stderr)
import Web.Stripe.Event
  ( Event (Event, eventData, eventId, eventType),
  )

run :: IO ()
run =
  WL.withStdoutLogger $ \aplogger -> do
      warpLogger <- jsonRequestLogger
      appLogger <- newStdoutLoggerSet defaultBufSize
      let ctx = AppCtx config appLogger
      
    let port = 8080
        settings =
          Warp.setPort port $
            Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
              Warp.setLogger aplogger $
                Warp.defaultSettings
    Warp.runSettings settings =<< server ctx

data Payload = Payload {id :: DT.Text} deriving (Show, Generic)

instance FromJSON Payload

someFunc :: Either IOError Body
someFunc =
  let body = Body $ DT.pack "{\"id\": 24353}"
      rawRequest = RawRequest body Nothing
   in verifySignature rawRequest

decode :: Body -> Either String Payload
decode body =
  eitherDecode $ encodeUtf8 $ DTL.pack $ DT.unpack $ fromBody body

newtype Body = Body DT.Text

fromBody :: Body -> DT.Text
fromBody (Body x) = x

data RawRequest = RawRequest {body :: Body, signature :: Maybe DT.Text}

verifySignature :: RawRequest -> Either IOError Body
verifySignature input =
  case signature input of
    (Just value) -> Right $ body input
    Nothing -> Right $ body input
