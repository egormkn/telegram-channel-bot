{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.JSON.Authorization where

import Data.Aeson
import GHC.Exts

import qualified Data.Text as Text

type ApiId = Integer
type ApiHash = String
type ApiKey = (ApiId, ApiHash)

stageOne :: ApiKey -> Value
stageOne (apiId, hash) = Object $ fromList [
    ("@type", String "setTdlibParameters"),
    ("parameters", Object $ fromList [
        ("database_directory", String "database"),
        ("use_message_database", Bool True),
        ("use_secret_chats", Bool True),
        ("api_id", Number $ fromInteger apiId),
        ("api_hash", String $ Text.pack hash),
        ("system_language_code", String "en"),
        ("device_model", String "Desktop"),
        ("system_version", String "Unknown"),
        ("application_version", String "0.1"),
        ("enable_storage_optimizer", Bool True)
    ])
  ]

stageTwo :: Value
stageTwo = Object $ fromList [
    ("@type", String "checkDatabaseEncryptionKey"),
    ("encryption_key", String "")
  ]

stageThree :: String -> Value
stageThree number = Object $ fromList  [
    ("@type", String "setAuthenticationPhoneNumber"),
    ("phone_number", String $ Text.pack number)
  ]
