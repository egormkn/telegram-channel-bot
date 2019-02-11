{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Channel
  (
    subscribeToChannel,
    getChannelId,
    getChat
  ) where

import Telegram.Database.Api.Decoding
import Telegram.Database.Api.Utils
import Telegram.Database.Json
import Data.Aeson
import GHC.Exts
import Data.Scientific
import Data.Text

import qualified Data.ByteString as ByteString


subscribeToChannel :: Client -> Integer -> IO ()
subscribeToChannel = joinChat

getChannelId :: Client -> Text -> IO Integer
getChannelId = searchPublicChat

joinChatJSON :: Integer -> Value
joinChatJSON chatId = Object $ fromList  [
  ("@type", String "joinChat"),
  ("chat_id", Number (scientific chatId 0))
  ]

searchPublicChatJSON :: Text -> Value
searchPublicChatJSON name = Object $ fromList  [
  ("@type", String "searchPublicChat"),
  ("username", String $ name)
  ]

getChatJSON :: Integer -> Value
getChatJSON chatId = Object $ fromList  [
  ("@type", String "getChat"),
  ("chat_id", Number (scientific chatId 0))
  ]

getChat :: Client -> Integer -> IO Text
getChat client chatId = do
  send client $ encodeValue $ getChatJSON chatId
  title <- waitForObjectWithType "chat" "title" client
  putStrLn "FFFFFFFUUUUUUUUUUUUUUUUCCCCCCCCCCCCCCCCKKKKKKKKKKK"
  putStrLn title
  print title
  print $ (read $ show title :: Text)
  return $ read $ show title

searchPublicChat :: Client -> Text -> IO Integer
searchPublicChat client channelName = do
  send client $ encodeValue $ searchPublicChatJSON channelName
  waitForObjectWithType "chat" "id" client

joinChat :: Client -> Integer -> IO ()
joinChat client chatId =
  send client $ encodeValue $ joinChatJSON chatId

waitForObjectWithType ::  FromJSON a => Text -> Text -> Client -> IO a
waitForObjectWithType typeStr fieldStr client = do
  message <- receive client
  printMessage message
  let field = getFieldFromMessage message
  helper field
  where
    getFieldFromMessage :: FromJSON a => Maybe ByteString.ByteString -> Result a
    getFieldFromMessage (Just str) = getFieldWithType typeStr fieldStr str
    getFieldFromMessage Nothing = Error "test"

    helper :: FromJSON a => Result a -> IO a
    helper (Success chatId) = return chatId
    helper _ = waitForObjectWithType typeStr fieldStr client

