{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Telegram.Database.Api.Messages where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Text
import GHC.Exts
import Telegram.Database.Json
import Telegram.Database.Api.Utils
import Telegram.Database.Api.Decoding

import qualified Data.ByteString as ByteString

telegramBaseLink :: Text
telegramBaseLink = "https://t.me/"

data Content = Content {
  contentType :: Text,
  msgText :: Maybe Text
} deriving (Show, Read, Eq)


data Message = Message {
  messageId :: Integer,
  chatId :: Integer,
  isChannelPost :: Bool,
  canBeForwarded :: Bool,
  content :: Content
} deriving (Show, Read, Eq)

containsTelegramLink :: Message -> Bool
containsTelegramLink Message { content = Content { msgText = Just msgText } } = telegramBaseLink `isPrefixOf` msgText
containsTelegramLink _ = False

getChannelNameFromText :: Text -> Maybe Text
getChannelNameFromText = stripPrefix telegramBaseLink

forwardMessageJSON :: Message -> Integer -> Value
forwardMessageJSON Message{messageId = msgId, chatId = fromChatId} chatId = Object $ fromList  [
  ("@type", String "forwardMessages"),
  ("from_chat_id", Number (scientific fromChatId 0)),
  ("chat_id", Number (scientific chatId 0)),
  ("message_ids", Array $ fromList [Number (scientific msgId 0)])
  ]

viewMessagesJSON :: Message -> Value
viewMessagesJSON Message{messageId = msgId, chatId = chatId} = Object $ fromList  [
  ("@type", String "viewMessages"),
  ("chat_id", Number (scientific chatId 0)),
  ("message_ids", Array $ fromList [Number (scientific msgId 0)]),
  ("force_read", Bool True)
  ]

sendMessageJSON :: Text -> Message -> Value
sendMessageJSON text Message{chatId = chatId} = Object $ fromList  [
  ("@type", String "sendMessage"),
  ("chat_id", Number (scientific chatId 0)),
  ("input_message_content", Object $ fromList [
    ("@type", String "inputMessageText"),
    ("text", Object $ fromList [
      ("@type", String "formattedText"),
      ("text", String text)
      ])
    ])
  ]

sendMessage :: Client -> Text -> Message -> IO ()
sendMessage client text msg = do
  print $ encodeValue $ sendMessageJSON text msg
  send client $ encodeValue $ sendMessageJSON text msg

forwardMessage :: Client -> Message -> Integer -> IO ()
forwardMessage client msg chatId =
  send client $ encodeValue $ forwardMessageJSON msg chatId

viewMessage :: Client -> Message -> IO ()
viewMessage client msg =
  send client $ encodeValue $ viewMessagesJSON msg


getNewMessage :: ByteString.ByteString -> Result Message
getNewMessage jsonStr = if isNewMessage then unpackState (getMessage maybeObj) else Error "Not new message"
  where
    maybeObj = getObject jsonStr
    type' = getTypeFromObject maybeObj
    isNewMessage = isNewMessageImpl type'

    unpackState :: Result (Result Message) -> Result Message
    unpackState (Success result) = result
    unpackState _ = Error "Can't parse message"

    getMessage :: Maybe Object -> Result (Result Message)
    getMessage (Just obj) = parse (\o -> do
      message <- o .: "message"
      return (fromJSON message))
      obj
    getMessage Nothing = Error "Can't parse message"

    isNewMessageImpl :: Result Text -> Bool
    isNewMessageImpl (Success "updateNewMessage") = True
    isNewMessageImpl _ = False


instance FromJSON Content where
  parseJSON = withObject "content" $ \o -> do
    contentType <- o .: "@type"
    msgText <- do
      textObj <- o .:? "text"
      case textObj of
        Just p -> p .:? "text"
        Nothing -> fail "doesn't have text"
    return $ Content{..}

instance FromJSON Message where
  parseJSON = withObject "new message" $ \o -> do
    messageId <- o .: "id"
    chatId <- o .: "chat_id"
    isChannelPost <- o .: "is_channel_post"
    canBeForwarded <- o .: "can_be_forwarded"
    content <- o .: "content"
    return $ Message {..}