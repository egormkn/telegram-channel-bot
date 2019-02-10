module Telegram.Database.Simple where

import Telegram.Database.Api.Channel
import Telegram.Database.Api.Messages
import Telegram.Database.Api.Utils
import Data.Aeson

import Telegram.Database.Json as TDLib
import qualified Data.ByteString as ByteString

process :: Client -> IO ()
process client = do
  message <- TDLib.receive client
  printMessage message
  tryProcessMessage message
  process client
  where
    tryProcessMessageImpl :: Result Message -> IO ()
    tryProcessMessageImpl (Success msg) | canBeForwarded msg && isChannelPost msg = do
      viewMessage client msg
      forwardMessage client msg 2115507
                                      | containsTelegramLink msg = tryToSubscribe channel
                                      | otherwise = print msg
                                      where
                                        channel = getChannelNameFromMessage msg
                                        tryToSubscribe :: Maybe String -> IO ()
                                        tryToSubscribe (Just name) = subscribeToChannel client name
                                        tryToSubscribe Nothing = return ()
                                        
    tryProcessMessageImpl (Error errMsg) = print $ "PARSE ERROR: " ++ errMsg

    tryProcessMessage :: Maybe ByteString.ByteString -> IO ()
    tryProcessMessage (Just message) = tryProcessMessageImpl (getNewMessage message)
    tryProcessMessage Nothing = return ()