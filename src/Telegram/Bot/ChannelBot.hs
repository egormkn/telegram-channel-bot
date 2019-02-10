{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.ChannelBot where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import Data.Int

type ChannelId = Int64
type ChannelUsername = Text

data Channel = Channel {
  channelId       :: ChannelId,
  channelTitle    :: Text,
  channelUsername :: ChannelUsername
} deriving (Show, Read, Eq)

makeChannel :: Channel
makeChannel = Channel { 
  channelId       = 0,
  channelTitle    = "",
  channelUsername = ""
}

data Model = Model { 
  channels   :: [Channel],
  silentMode :: Bool
}

makeModel :: Model
makeModel = Model { 
  channels   = [], 
  silentMode = False
}

addChannel :: ChannelUsername -> Model -> Model
addChannel name model = model { 
  channels = makeChannel { channelUsername = name } : channels model
}

removeChannel :: ChannelUsername -> Model -> Model
removeChannel name model = model { 
  channels = filter ((/= name) . channelUsername) $ channels model
}

data Action
  = NoOp
  | Start
  | AddChannel ChannelUsername
  | ShowChannel ChannelUsername
  | ToggleChannel ChannelUsername
  | RemoveChannel ChannelUsername
  | ShowChannels
  | SilentMode
  | Forward Message
  | Debug
  | DebugMessage Text
  deriving (Show)

channelBot :: BotApp Model Action
channelBot = BotApp { 
  botInitialModel = makeModel,
  botAction = flip updateToAction,
  botHandler = handleAction,
  botJobs = [backgroundJob]
}

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update = do
  message <- updateMessage update
  case messageForwardFromChat message of
    Just chat -> Just $ Forward message
    Nothing -> parseUpdate (
          Start         <$  command "start"
      <|> AddChannel    <$> command "add"
      <|> ShowChannel   <$> command "show"
      <|> RemoveChannel <$> command "remove"
      <|> ShowChannels  <$  command "showall"
      <|> SilentMode    <$  command "silent"
      <|> Debug         <$  command "debug"
      <|> DebugMessage  <$> text) update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  Start -> model <# do
    replyText startMessage
    return NoOp
  AddChannel name -> addChannel name model <# do
    replyText "Ok, got it!"
    return NoOp
  ShowChannel name -> model <# do
    reply (toReplyMessage $ Text.append "Show channel " name)
      { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
    return NoOp
  RemoveChannel name -> removeChannel name model <# do
    replyText "Item removed!"
    return NoOp
  ToggleChannel name -> model <# do
    return $ if name `elem` (map channelUsername $ channels model) then RemoveChannel name else AddChannel name 
  ShowChannels -> model <# do
    reply (toReplyMessage "Available channels. Click to remove")
      { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
    return NoOp
  Debug -> model <# do
    replyText "Debug info"
    currentId <- currentChatId
    me <- liftClientM getMe
    replyText $ getChatId currentId
    replyText $ Text.pack $ show me
    return NoOp
  DebugMessage msg -> model <# do
    replyText msg
    return NoOp
  Forward msg -> model <# do
    forwardTo (SomeChatId (ChatId 2115507)) msg
    return NoOp
  SilentMode -> model { silentMode = not $ silentMode model } <# do
    replyText "Silent mode enabled!"
    return NoOp

  where
    listsKeyboard = InlineKeyboardMarkup $
      map ((\name -> [actionButton name (RemoveChannel name)]) . channelUsername) (channels model)

getChatId :: Maybe ChatId -> Text
getChatId Nothing = "Unknown"
getChatId (Just i) = Text.pack $ show i

startMessage :: Text
startMessage = Text.unlines [
  "Hello! I am a bot that allows to merge different channels into one :)",
  "",
  "You can add and remove channel subscriptions just by typing it's Telegram username!",
  "You can also use `/add channelname` command to do that explicitly.",
  "To remove a channel use `/remove channelname` command.",
  "",
  "Show all channels with /showall.",
  "Show information about a specific channel with `/show channelname`."
  ]

backgroundJob :: BotJob Model Action
backgroundJob = BotJob {
  botJobSchedule = "* * * * *",
    -- ^ Cron schedule for the job.
  botJobTask = backgroundTask
    -- ^ Job function.
}

backgroundTask :: Model -> Eff Action Model
backgroundTask model = model <# do
  replyTo (SomeChatId (ChatId 2115507)) (toReplyMessage "Hello from CRON task")
    { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
  return NoOp
  where
    listsKeyboard = InlineKeyboardMarkup $
      map ((\name -> [actionButton name (ShowChannel name)]) . channelUsername) (channels model)

-- run :: String -> IO ()
-- run token = do
--   token <- Token . Text.pack <$> token
--   defaultTelegramClientEnv >=> startBot_ (conversationBot updateChatId channelBot)
