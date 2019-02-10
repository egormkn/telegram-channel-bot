{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Simple where

import Telegram.Bot.Parser
import Telegram.Database.Api.Channel
import qualified Telegram.Database.Api.Messages as Messages
import Telegram.Database.Api.Utils
import Data.Aeson
import Data.Text

import Telegram.Database.Json as TDLib
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict

import Control.Applicative
import Control.Monad

type UserToChannels = HashMap Integer [Integer]
type ChannelToUsers = HashMap Integer [Integer]
type State = (UserToChannels, ChannelToUsers)

data Action = 
    AddChannel Text
  | RemoveChannel Text
  | ShowChannels

process :: State -> Client -> IO ()
process state client = do
  message <- TDLib.receive client
  printMessage message
  (newUserToChannels, newChannelToUsers) <- tryProcessMessage state message
  process (newUserToChannels, newChannelToUsers) client
  where
    tryProcessMessageImpl :: State -> Result Messages.Message -> IO State
    tryProcessMessageImpl state' (Success msg) 
      | Messages.canBeForwarded msg && Messages.isChannelPost msg = do
        Messages.viewMessage client msg
        forward state' client msg
        return state'
      -- | containsTelegramLink msg =
      --   tryToSubscribe state' client msg channel
      | otherwise = processCommand state' client msg
      -- where
      --   channel = Messages.getChannelNameFromText $ pack $ Messages.text msg
                                        
                                        
    tryProcessMessageImpl state (Error errMsg) = do
      print $ "PARSE ERROR: " ++ errMsg
      return state

    tryProcessMessage :: State -> Maybe ByteString.ByteString -> IO State
    tryProcessMessage (userToChannels, channelToUsers) (Just message) = tryProcessMessageImpl (userToChannels, channelToUsers) (Messages.getNewMessage message)
    tryProcessMessage (userToChannels, channelToUsers) Nothing = return (userToChannels, channelToUsers)

tryToSubscribe :: State -> Client -> Messages.Message -> Maybe Text -> IO State
tryToSubscribe (userToChannels, channelToUsers) client Messages.Message{Messages.chatId=userChatId} (Just name) = do
  channelId <- getChannelId client $ unpack name
  newUserToChannels <- return $ insertWith (++) userChatId [channelId] userToChannels
  newChannelToUsers <- return $ insertWith (++) channelId [userChatId] channelToUsers
  subscribeToChannel client channelId

  print newUserToChannels
  print newChannelToUsers
  return (newUserToChannels, newChannelToUsers)
tryToSubscribe (userToChannels, channelToUsers) _  _ Nothing = return (userToChannels, channelToUsers)

forward :: State -> Client -> Messages.Message -> IO ()
forward (_, channelToUsers) client msg@Messages.Message{Messages.chatId=channelId} = do
  users <- return $ channelToUsers ! channelId
  mapM_ (\userId -> Messages.forwardMessage client msg userId) users

processCommand :: State -> Client -> Messages.Message -> IO State
processCommand state client Messages.Message { Messages.msgText = Nothing } = return state
processCommand state client msg@Messages.Message { Messages.msgText = Just Messages.MsgText { Messages.text = text } } = do
  modifyState $ parse $ pack text
  where
    parse :: Text -> Maybe (Action, Text)
    parse = parseCommand (
            AddChannel    <$> command "add"
        <|> RemoveChannel <$> command "remove"
        <|> ShowChannels  <$  command "show"
      )

    modifyState :: Maybe (Action, Text) -> IO State
    modifyState (Just (action, other)) = modifyStateImpl action other
    modifyState Nothing = return state

    modifyStateImpl :: Action -> Text -> IO State
    modifyStateImpl (AddChannel t) other = tryToSubscribe state client msg $ Messages.getChannelNameFromText other