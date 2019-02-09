{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api where

import Telegram.Database.Json as TDLib
import GHC.Exts
import Data.Aeson

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as ByteString.Lazy

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment

type ApiId = Integer
type ApiHash = String
type ApiKey = (ApiId, ApiHash)

{-# LANGUAGE RecordWildCards #-}

data TdlibParameters = TdlibParameters {
    database_directory :: String, 
    use_message_database :: Bool,
    use_secret_chats :: Bool,
    api_id :: Int,
    api_hash :: String,
    system_language_code :: String,
    device_model :: String,
    system_version :: String,
    application_version :: String,
    enable_storage_optimizer :: String
  }

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    age  <- o .: "age"
    return Person{..}

instance ToJSON Person where
  toJSON Person{..} = object [
    "name" .= name,
    "age"  .= age  ]

stageOne :: ApiKey -> Value
stageOne (id, hash) = Object $ fromList [
    ("@type", String "setTdlibParameters"),
    ("parameters", Object $ fromList [
        ("database_directory", String "database"),
        ("use_message_database", Bool True),
        ("use_secret_chats", Bool True),
        ("api_id", Number $ fromInteger id),
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

printLoop :: Client -> IO ()
printLoop client = do
  message4 <- TDLib.receive client
  print message4
  printLoop client

authorize :: ApiKey -> IO Client
authorize key = do
  _ <- loadFile defaultConfig
  value <- getEnv "API_ID"
  print value
  client <- TDLib.create
  TDLib.send client "{\"@type\": \"getAuthorizationState\", \"@extra\": 1.01234}"
  message <- TDLib.receive client
  print message
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageOne key
  message2 <- TDLib.receive client
  print message2
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageTwo
  message3 <- TDLib.receive client
  print message3
  putStrLn "Please, enter mobile phone number:"
  number <- getLine
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageThree number
  printLoop client
  return client

destroy :: Client -> IO ()
destroy = TDLib.destroy

-- prosessResponse :: Maybe String -> IO ()
-- processResponse Nothing = return ()
-- processResponse (Just response)
--   | 