{-# LANGUAGE OverloadedStrings #-}

module Main where

import Telegram.Database.Api.Authorization
import Telegram.Database.Simple
import Data.HashMap.Strict

import qualified Configuration.Env as Env

main :: IO ()
main = do
  Env.load
  apiId <- Env.get "Telegram API ID" "API_ID"
  apiHash <- Env.get "Telegram API hash" "API_HASH"
  client <- authorize (read apiId, apiHash)
  print ">>>>> AUTHORIZED <<<<<"
  process (empty, empty) client
  print ">>>>> DESTROYED <<<<<"
  close client
  return ()

