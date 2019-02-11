{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Decoding (
  getField,
  getFieldFromObject,
  getType,
  getTypeFromObject,
  getObject,
  getFieldWithType
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.ByteString as ByteString

getType :: ByteString.ByteString -> Result Text
getType = getField "@type"

getTypeFromObject :: Maybe Object -> Result Text
getTypeFromObject = getFieldFromObject "@type"

getField :: FromJSON a => Text -> ByteString.ByteString -> Result a
getField fieldName str = getFieldFromObject fieldName $ getObject str

getFieldFromObject :: FromJSON a => Text -> Maybe Object -> Result a
getFieldFromObject _ Nothing = Error "can't parse"
getFieldFromObject fieldName (Just obj) = parse helper obj
  where
    helper :: FromJSON a => Object -> Parser a
    helper o = o .: fieldName

getObject :: ByteString.ByteString -> Maybe Object
getObject str = decodeStrict str :: Maybe Object

getFieldWithType :: FromJSON a => Text -> Text -> ByteString.ByteString -> Result a
getFieldWithType typeStr fieldStr jsonStr = if isChat then getFieldFromObject fieldStr maybeObj else Error "Not updateAuthorizationState"
  where
    maybeObj = getObject jsonStr
    type' = getTypeFromObject maybeObj
    isChat = isChatImpl type'

    isChatImpl :: Result Text -> Bool
    isChatImpl (Success str)  | str == typeStr = True
                              | otherwise = False
    isChatImpl _ = False