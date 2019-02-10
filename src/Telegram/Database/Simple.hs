module Telegram.Database.Simple where

import Telegram.Database.Api.Channel
import Telegram.Database.Api.Messages
import Telegram.Database.Api.Utils
import Data.Aeson

import Telegram.Database.Json as TDLib
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict

type UserToChannels = HashMap Integer [Integer]
type ChannelToUsers = HashMap Integer [Integer]
type State = (UserToChannels, ChannelToUsers)

process :: State -> Client -> IO ()
process state client = do
  message <- TDLib.receive client
  printMessage message
  (newUserToChannels, newChannelToUsers) <- tryProcessMessage state message
  process (newUserToChannels, newChannelToUsers) client
  where
    tryProcessMessageImpl :: State -> Result Message -> IO State
    tryProcessMessageImpl state' (Success msg) | canBeForwarded msg && isChannelPost msg = do
      viewMessage client msg
      forward state' client msg
      return state'
        | containsTelegramLink msg =
          tryToSubscribe state' client msg channel
        | otherwise = do
          print msg
          return state'
        where
          channel = getChannelNameFromMessage msg
                                        
                                        
    tryProcessMessageImpl (userToChannels, channelToUsers) (Error errMsg) = do
      print $ "PARSE ERROR: " ++ errMsg
      return (userToChannels, channelToUsers)

    tryProcessMessage :: State -> Maybe ByteString.ByteString -> IO State
    tryProcessMessage (userToChannels, channelToUsers) (Just message) = tryProcessMessageImpl (userToChannels, channelToUsers) (getNewMessage message)
    tryProcessMessage (userToChannels, channelToUsers) Nothing = return (userToChannels, channelToUsers)

tryToSubscribe :: State -> Client -> Message -> Maybe String -> IO State
tryToSubscribe (userToChannels, channelToUsers) client Message{chatId=userChatId} (Just name) = do
  channelId <- getChannelId client name
  newUserToChannels <- return $ insertWith (++) userChatId [channelId] userToChannels
  newChannelToUsers <- return $ insertWith (++) channelId [userChatId] channelToUsers
  subscribeToChannel client channelId

  print newUserToChannels
  print newChannelToUsers
  return (newUserToChannels, newChannelToUsers)
tryToSubscribe (userToChannels, channelToUsers) _  _ Nothing = return (userToChannels, channelToUsers)

forward :: State -> Client -> Message -> IO ()
forward (_, channelToUsers) client msg@Message{chatId=channelId} = do
  users <- return $ channelToUsers ! channelId
  mapM_ (\userId -> forwardMessage client msg userId) users