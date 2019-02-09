{-# LANGUAGE OverloadedStrings #-}

module JSON.Decoding
 (
  getType
 )
 where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

getType :: TL.Text -> Result String
getType str = getTypeFromObject $ getObject str

getTypeFromObject :: Maybe Object -> Result String
getTypeFromObject (Just obj) = parse helper obj
  where
    helper :: Object -> Parser String
    helper o = do
      _type <- o .: "@type"
      return _type
getTypeFromObject Nothing = Error "can't parse"

getObject :: TL.Text -> Maybe Object
getObject str = decode (TL.encodeUtf8 str) :: Maybe Object