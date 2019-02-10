{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Channel
  (
    subscribeToChannel,
    getChannelId
  ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Data.Scientific
import Telegram.Database.Api.Decoding
import Telegram.Database.Api.Utils

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Telegram.Database.Json as TDLib
import qualified Data.Text as Text

subscribeToChannel :: TDLib.Client -> Integer -> IO ()
subscribeToChannel = joinChat

getChannelId :: TDLib.Client -> String -> IO Integer
getChannelId = searchPublicChat

joinChatJSON :: Integer -> Value
joinChatJSON chatId = Object $ fromList  [
  ("@type", String "joinChat"),
  ("chat_id", Number (scientific chatId 0))
  ]

searchPublicChatJSON :: String -> Value
searchPublicChatJSON name = Object $ fromList  [
  ("@type", String "searchPublicChat"),
  ("username", String $ Text.pack name)
  ]

searchPublicChat :: TDLib.Client -> String -> IO Integer
searchPublicChat client channelName = do
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ searchPublicChatJSON channelName
  waitForChatID client
  
joinChat :: TDLib.Client -> Integer -> IO ()
joinChat client chatId =
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ joinChatJSON chatId

waitForChatID :: TDLib.Client -> IO Integer
waitForChatID client = do
  message <- TDLib.receive client
  printMessage message
  id <- return $ getChatIdFromMessage message
  helper id
  where
    getChatIdFromMessage :: Maybe ByteString.ByteString -> Result Integer
    getChatIdFromMessage (Just str) = getChatID str
    getChatIdFromMessage Nothing = Error "test"

    helper :: Result Integer -> IO Integer
    helper (Success chatId) = return chatId
    helper _ = waitForChatID client

getChatID :: ByteString.ByteString -> Result Integer
getChatID jsonStr = if isChat then getChatImpl obj else Error "Not updateAuthorizationState"
  where
    obj = getObject jsonStr
    type' = getTypeFromObject obj
    isChat = isChatImpl type'
 
    getChatImpl :: (Maybe Object) -> Result Integer
    getChatImpl (Just obj) = parse (\o -> o .: "id") obj
    getChatImpl Nothing = Error "Can't parse authorization_state"
 
    isChatImpl :: Result String -> Bool
    isChatImpl (Success "chat") = True
    isChatImpl _ = False