{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.TodoBot where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Item = Text

data Model = Model
  { todoLists   :: HashMap Text [Item]
  , currentList :: Text
  }

defaultListName :: Text
defaultListName = "Default"

initialModel :: Model
initialModel = Model
  { todoLists = HashMap.fromList [(defaultListName, [])]
  , currentList = defaultListName
  }

data Action
  = NoOp
  | Start
  | AddItem Item
  | RemoveItem Item
  | SwitchToList Text
  | ShowAll
  | Show Text
  deriving (Show, Read)

todoBot :: BotApp Model Action
todoBot = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          AddItem      <$> plainText
      <|> Start        <$  command "start"
      <|> AddItem      <$> command "add"
      <|> RemoveItem   <$> command "remove"
      <|> SwitchToList <$> command "switch_to_list"
      <|> Show         <$> command "show"
      <|> ShowAll      <$  command "show_all"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      Start -> model <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
        return NoOp
      AddItem item -> addItem item model <# do
        replyText "Ok, got it!"
        return NoOp
      RemoveItem item -> removeItem item model <# do
        replyText "Item removed!"
        return NoOp
      SwitchToList name -> model { currentList = name } <# do
        replyText ("Switched to list «" <> name <> "»!")
        return NoOp
      ShowAll -> model <# do
        reply (toReplyMessage "Available todo lists")
          { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup listsKeyboard) }
        return NoOp
      Show "" -> model <# do
        return (Show defaultListName)
      Show name -> model <# do
        let items = concat (HashMap.lookup name (todoLists model))
        if null items
          then reply (toReplyMessage ("The list «" <> name <> "» is empty. Maybe try these starter options?"))
                 { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
          else replyText (Text.unlines items)
        return NoOp

      where
        listsKeyboard = InlineKeyboardMarkup
          (map (\name -> [actionButton name (Show name)]) (HashMap.keys (todoLists model)))

    startMessage = Text.unlines
      [ "Hello! I am your personal TODO bot :)"
      , ""
      , "You can add new items to your todo list just by typing it!"
      , "You can also use /add command to do that explicitely."
      , "To remove an item use /remove command."
      , ""
      , "You can manage multiple todo lists:"
      , "Switch to a new named list with /switch_to_list <list>."
      , "Show all available lists with /show_all."
      , "Show items for a specific list with /show <list>."
      , ""
      , "Here are some starter options, try adding something to the list."
      ]

    startKeyboard :: ReplyKeyboardMarkup
    startKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
          [ [ "Buy milk", "Get job done" ]
          , [ "Build a house", "Plant a tree" ]
          ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }

addItem :: Item -> Model -> Model
addItem item model = model
  { todoLists = HashMap.insertWith (++) (currentList model) [item] (todoLists model) }

removeItem :: Item -> Model -> Model
removeItem item model = model
  { todoLists = HashMap.adjust (filter (/= item)) (currentList model) (todoLists model)}

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId todoBot) env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
  return ()
