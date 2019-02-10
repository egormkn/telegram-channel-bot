{-# LANGUAGE OverloadedStrings #-}

module Main where

import Telegram.Bot.TodoBot (todoBot)
import qualified Data.Text as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Database.Api.Authorization
import qualified Configuration.Env as Env
import Control.Monad

run :: Token -> IO ()
run = defaultTelegramClientEnv >=> startBot_ (conversationBot updateChatId todoBot)

main :: IO ()
main = do
  Env.load
  api_id <- read <$> Env.get "Telegram API ID" "API_ID"
  api_hash <- Env.get "Telegram API hash" "API_HASH"
  client <- authorize (api_id, api_hash)
  close client
  -- token <- Token . Text.pack <$> Env.get "Telegram Bot API token" "BOT_TOKEN"
  -- run token
  return ()

