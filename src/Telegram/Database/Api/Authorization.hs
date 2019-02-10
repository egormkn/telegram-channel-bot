{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Telegram.Database.Api.Authorization where

import Telegram.Database.Api.Decoding
import Data.Aeson
import GHC.Exts

import qualified Telegram.Database.Json as TDLib
import qualified Data.Text as Text

import Telegram.Database.Json as TDLib

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString as ByteString

import qualified Configuration.Env as Env

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

instance FromJSON TdlibParameters where
  parseJSON = withObject "parameters" $ \o -> do
    database_directory   <- o .: "database_directory"
    use_message_database <- o .: "use_message_database"
    return TdlibParameters{..}

instance ToJSON TdlibParameters where
  toJSON TdlibParameters{..} = object [
    "database_directory" .= database_directory,
    "use_message_database"  .= use_message_database  ]

-- setTdlibParameters :: TdlibParameters -> IO ()
-- setTdlibParameters p = do
--   TDLib.send client $ ByteString.Lazy.toStrict $ encode $ toJSON $ TdlibParameters {}

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

stageFour :: String -> Value
stageFour number = Object $ fromList  [
    ("@type", String "checkAuthenticationCode"),
    ("code", String $ Text.pack number)
  ]

printMessage :: Maybe ByteString.ByteString -> IO ()
printMessage (Just message) = print message
printMessage Nothing = return ()

printLoop :: Client -> IO ()
printLoop client = do
  message4 <- TDLib.receive client
  printMessage message4
  printLoop client

getAuthStateFromMessage :: Maybe ByteString.ByteString -> Result AuthorizationState
getAuthStateFromMessage (Just str) = getAuthorizationState str
getAuthStateFromMessage Nothing = Error "test"

checkAuthorizationState :: AuthorizationState -> Result AuthorizationState -> Bool
checkAuthorizationState expextedState (Success state) =  expextedState == state
checkAuthorizationState _ _ = False

waitForType :: AuthorizationState -> Client -> IO ()
waitForType expectedState client = do
  message <- TDLib.receive client
  print ("WAIT: Wating for " ++ show expectedState)
  state <- (return $ getAuthStateFromMessage message)
  printMessage message
  helper state
  where
    helper :: Result AuthorizationState -> IO ()
    helper (Success state) = do
      print ("########## Type recieved " ++ show state ++ " ##########")
      processState client state
    helper _ = waitForType expectedState client


type ApiId = Integer
type ApiHash = String
type ApiKey = (ApiId, ApiHash)

processState :: Client -> AuthorizationState -> IO ()
processState client AuthorizationStateWaitEncryptionKey = do
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageTwo
  waitForType AuthorizationStateWaitPhoneNumber client
processState client AuthorizationStateWaitPhoneNumber = do
  number <- Env.get "mobile phone number" "PHONE_NUMBER"
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageThree number
  waitForType AuthorizationStateWaitCode client
processState client AuthorizationStateWaitCode = do
  putStrLn "Please, enter code:"
  code <- getLine
  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageFour code
  waitForType AuthorizationStateReady client
processState _ AuthorizationStateReady = return ()
processState client _ = waitForType AuthorizationStateReady client

authorize :: ApiKey -> IO Client
authorize key = do
  client <- TDLib.create
  TDLib.send client "{\"@type\": \"getAuthorizationState\", \"@extra\": 1.01234}"

  TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageOne key
  waitForType AuthorizationStateWaitEncryptionKey client

  -- TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageTwo
  -- waitForType AuthorizationStateWaitPhoneNumber client
  -- number <- Env.get "mobile phone number" "PHONE_NUMBER"

  -- TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageThree number
  -- waitForType AuthorizationStateWaitCode client

  -- putStrLn "Please, enter code:"
  -- code <- getLine
  -- TDLib.send client $ ByteString.Lazy.toStrict $ encode $ stageFour code

  print "!!!!!!!!!!!!!!! LOOP STARTED !!!!!!!!!!!!!!"
  printLoop client
  return client

close :: Client -> IO ()
close = TDLib.destroy