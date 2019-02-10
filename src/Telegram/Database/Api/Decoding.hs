{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Decoding (getType) where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

getType :: TL.Text -> Result String
getType str = getTypeFromObject $ getObject str

getTypeFromObject :: Maybe Object -> Result String
getTypeFromObject Nothing = Error "can't parse"
getTypeFromObject (Just obj) = parse helper obj
  where
    helper :: Object -> Parser String
    helper o = o .: "@type"

getObject :: TL.Text -> Maybe Object
getObject str = decode (TL.encodeUtf8 str) :: Maybe Object