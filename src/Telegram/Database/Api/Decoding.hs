{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Decoding (
  getField,
  getFieldFromObject,
  getType,
  getTypeFromObject,
  getObject
  ) where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

getType :: ByteString.ByteString -> Result String
getType = getField "@type"

getTypeFromObject :: Maybe Object -> Result String
getTypeFromObject = getFieldFromObject "@type"

getField :: Text.Text -> ByteString.ByteString -> Result String
getField fieldName str = getFieldFromObject fieldName $ getObject str

getFieldFromObject :: Text.Text -> Maybe Object -> Result String
getFieldFromObject _ Nothing = Error "can't parse"
getFieldFromObject fieldName (Just obj) = parse helper obj
  where
    helper :: Object -> Parser String
    helper o = o .: fieldName

getObject :: ByteString.ByteString -> Maybe Object
getObject str = decodeStrict str :: Maybe Object