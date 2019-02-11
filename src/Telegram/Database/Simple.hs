{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Simple (process) where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict
import Telegram.Database.Api.Channel
import Telegram.Database.Api.Utils
import Text.Read

import Data.Text as Text
import Logging as Log
import Telegram.Database.Json as TDLib
import Telegram.Database.Api.Messages

import Telegram.Bot.MessageParser

import qualified Data.ByteString as ByteString

type UserToChannels = HashMap Integer [Integer]
type ChannelToUsers = HashMap Integer [Integer]
type State = (UserToChannels, ChannelToUsers)

tag :: Text
tag = "SIMPLE"

data Action = 
    AddChannel Text
  | RemoveChannel Text
  | ShowChannels
  | Unknown Text
  deriving(Show)

process :: State -> Client -> IO ()
process state client = do
  message <- receive client
  printMessage message
  (newUserToChannels, newChannelToUsers) <- tryProcessMessage message
  process (newUserToChannels, newChannelToUsers) client
  where
    tryProcessMessageImpl :: Result Message -> IO State
    tryProcessMessageImpl (Success msg) 
      | canBeForwarded msg && isChannelPost msg = do
        viewMessage client msg
        forward state client msg
        return state
      -- | containsTelegramLink msg =
      --   tryToSubscribe state' client msg channel
      | otherwise = processCommand state client msg
      -- where
      --   channel = getChannelNameFromText $ pack $ text msg
                                        
                                        
    tryProcessMessageImpl (Error errMsg) = do
      print $ "PARSE ERROR: " ++ errMsg
      return state

    tryProcessMessage :: Maybe ByteString.ByteString -> IO State
    tryProcessMessage (Just message) = tryProcessMessageImpl (getNewMessage message)
    tryProcessMessage Nothing = return state

forward :: State -> Client -> Message -> IO ()
forward (_, channelToUsers) client msg@Message{chatId=channelId} = do
  print channelId
  print channelToUsers
  users <- return $ channelToUsers ! channelId
  mapM_ (forwardMessage client msg) users

processCommand :: State -> Client -> Message -> IO State
processCommand state _ msg@Message { content = Content { msgText = Nothing} } = return state
processCommand state client msg = do
  Log.logWithTag tag "processCommand"
  Log.logWithTag tag $ pack (show msg)
  let parseResult = parse $ msgText $ content msg

  Log.logWithTag tag $ pack ("Parse result: " ++ show parseResult)

  modifyState parseResult
  where
    parse :: Maybe Text -> Maybe Action
    parse Nothing = Nothing
    parse (Just commandText) = parseMessage (
            AddChannel    <$> command "add"
        <|> RemoveChannel <$> command "remove"
        <|> ShowChannels  <$  command "show"
        <|> Unknown       <$> text
      ) commandText

    modifyState :: Maybe Action -> IO State
    modifyState (Just action) = do
      Log.logWithTag tag "modifyState"
      modifyStateImpl action
    modifyState Nothing = do
      Log.logWithTag tag "modifyState"
      return state

    modifyStateImpl :: Action -> IO State
    modifyStateImpl (AddChannel t) = do
      Log.logWithTag tag "AddChannel"
      tryToSubscribe state client msg $ getChannelNameFromText t
    modifyStateImpl ShowChannels = do
      Log.logWithTag tag "ShowChannels"
      showUserChannels state client msg
      return state
    modifyStateImpl (RemoveChannel channelId) =
      deleteChannel state (readMaybe $ unpack channelId) msg
    modifyStateImpl _ = do
      Log.logWithTag tag "Unknown command"
      return state

deleteChannel :: State -> Maybe Integer -> Message -> IO State
deleteChannel (userToChannels, channelToUsers) (Just channelId) Message{chatId=userChatId} = do
  let newUserToChannels = adjust (Prelude.filter (/= channelId)) userChatId userToChannels
  let newChannelToUsers = adjust (Prelude.filter (/= userChatId)) channelId channelToUsers
  print newUserToChannels
  print newChannelToUsers
  return (newUserToChannels, newChannelToUsers)
deleteChannel state Nothing _ = return state


showUserChannels :: State -> Client -> Message -> IO ()
showUserChannels (userToChannels, _) client msg@Message{chatId=userChatId} = do
  let channelIds = userToChannels ! userChatId
  print channelIds

  let channelReq = fmap (getChat client) channelIds
  channelNames <- sequence channelReq
  let channels = Prelude.zip channelNames channelIds
  print channels

  let answer = Text.concat (Prelude.map (\p -> pack $ show p ++ "\n") channels)

  sendMessage client answer msg

tryToSubscribe :: State -> Client -> Message -> Maybe Text -> IO State
tryToSubscribe (userToChannels, channelToUsers) client Message{chatId=userChatId} (Just name) = do
  print name
  channelId <- getChannelId client name
  let newUserToChannels = insertWith (++) userChatId [channelId] userToChannels
  let newChannelToUsers = insertWith (++) channelId [userChatId] channelToUsers
  subscribeToChannel client channelId

  print newUserToChannels
  print newChannelToUsers
  return (newUserToChannels, newChannelToUsers)
tryToSubscribe (userToChannels, channelToUsers) _  _ Nothing = return (userToChannels, channelToUsers)