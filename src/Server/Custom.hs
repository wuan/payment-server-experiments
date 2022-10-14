{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Custom (JsonAsOctetStream) where

import qualified Data.ByteString as DB (ByteString)
import qualified Data.ByteString.Lazy as DBL
  ( ByteString,
    fromStrict,
    toStrict,
  )
import qualified Data.Data as DD (Typeable)
import qualified Data.List.NonEmpty as NE
import qualified Network.HTTP.Media as M
import qualified Servant as S
  ( Accept (..),
    MimeRender (mimeRender),
    MimeUnrender (mimeUnrender),
  )

data JsonAsOctetStream deriving (DD.Typeable)

instance S.Accept JsonAsOctetStream where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance S.MimeRender JsonAsOctetStream DBL.ByteString where
  mimeRender _ = id

instance S.MimeRender JsonAsOctetStream DB.ByteString where
  mimeRender _ = DBL.fromStrict

instance S.MimeUnrender JsonAsOctetStream DBL.ByteString where
  mimeUnrender _ = Right

instance S.MimeUnrender JsonAsOctetStream DB.ByteString where
  mimeUnrender _ = Right . DBL.toStrict
