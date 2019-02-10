{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Telegram.Database.Api.Messages where

import Telegram.Database.Api.Decoding
import GHC.Exts
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.List

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Telegram.Database.Json as TDLib

telegramBaseLink :: String
telegramBaseLink = "https://t.me/"

data MsgText = MsgText {
  type' :: String,
  text :: String
} deriving (Show, Read, Eq)

instance FromJSON MsgText where
  parseJSON = withObject "message text" $ \o -> do
    type' <- o .: "@type" 
    text <- o .: "text"
    return $ MsgText{..}

data Message = Message {
  id :: Integer,
  chatId :: Integer,
  isChannelPost :: Bool,
  canBeForwarded :: Bool,
  msgText :: MsgText
} deriving (Show, Read, Eq)

instance FromJSON Message where
  parseJSON = withObject "new message" $ \o -> do
    id <- o .: "id" :: (Parser Integer)
    chatId <- o .: "chat_id" :: (Parser Integer)
    isChannelPost <- o .: "is_channel_post" :: (Parser Bool)
    canBeForwarded <- o .: "can_be_forwarded" :: (Parser Bool)
    content <- o .: "content"
    msgText <- content .: "text"
    return $ Message {..}

containsTelegramLink :: Message -> Bool 
containsTelegramLink Message{msgText=MsgText{text=text}} = isTelegramLink text
  where
    isTelegramLink :: String -> Bool
    isTelegramLink = isPrefixOf telegramBaseLink

getChannelNameFromMessage :: Message -> Maybe String
getChannelNameFromMessage Message{msgText=MsgText{text=text}} = getChannelName text
  where
    getChannelName :: String -> Maybe String
    getChannelName = stripPrefix telegramBaseLink


forwardMessageJSON :: Message -> Integer -> Value
forwardMessageJSON Message{id = msgId, chatId = fromChatId} chatId = Object $ fromList  [
  ("@type", String "forwardMessages"),
  ("from_chat_id", Number (scientific fromChatId 0)),
  ("chat_id", Number (scientific chatId 0)),
  ("message_ids", Array $ fromList [Number (scientific msgId 0)])
  ]

viewMessagesJSON :: Message -> Value
viewMessagesJSON Message{id = msgId, chatId = chatId} = Object $ fromList  [
  ("@type", String "viewMessages"),
  ("chat_id", Number (scientific chatId 0)),
  ("message_ids", Array $ fromList [Number (scientific msgId 0)]),
  ("force_read", Bool True)
  ]

forwardMessage :: TDLib.Client -> Message -> Integer -> IO ()
forwardMessage client msg chatId =
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ forwardMessageJSON msg chatId

viewMessage :: TDLib.Client -> Message -> IO ()
viewMessage client msg =
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ viewMessagesJSON msg


getNewMessage :: ByteString.ByteString -> Result Message
getNewMessage jsonStr = if isNewMessage then unpackState (getMessage obj) else Error "Not new message"
  where
    obj = getObject jsonStr
    type' = getTypeFromObject obj
    isNewMessage = isNewMessageImpl type'

    unpackState :: Result (Result Message) -> Result Message
    unpackState (Success result) = result
    unpackState _ = Error "Can't parse message id"

    getMessage :: (Maybe Object) -> Result (Result Message)
    getMessage (Just obj) = parse (\o -> do
      message <- o .: "message"
      return (fromJSON message))
      obj
    getMessage Nothing = Error "Can't parse message id"

    isNewMessageImpl :: Result String -> Bool
    isNewMessageImpl (Success "updateNewMessage") = True
    isNewMessageImpl _ = False