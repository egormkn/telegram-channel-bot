module Telegram.Database.Api.Utils where

import qualified Data.ByteString as ByteString

printMessage :: Maybe ByteString.ByteString -> IO ()
printMessage (Just message) = print message
printMessage Nothing = return ()