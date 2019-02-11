{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Telegram.Database.Api.Authorization
import Telegram.Database.Simple

import Logging as Log
import Data.HashMap.Strict as HM
import qualified Configuration.Env as Env

tag :: Text
tag = "MAIN"

main :: IO ()
main = do
  Env.load
  apiId <- Env.get "Telegram API ID" "API_ID"
  apiHash <- Env.get "Telegram API hash" "API_HASH"
  client <- authorize (read apiId, apiHash)
  Log.logWithTag tag ">>>>> AUTHORIZED <<<<<"
  process (HM.empty, HM.empty) client
  Log.logWithTag tag ">>>>> DESTROYED <<<<<"
  close client
  return ()

