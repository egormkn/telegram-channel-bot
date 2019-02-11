module Telegram.Database.Api.Utils where

import Data.Aeson

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy

printMessage :: Maybe ByteString.ByteString -> IO ()
printMessage (Just message) = print message
printMessage Nothing = return ()

encodeValue :: Value-> ByteString.ByteString
encodeValue =  ByteString.Lazy.toStrict . encode