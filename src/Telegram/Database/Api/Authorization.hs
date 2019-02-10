{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Telegram.Database.Api.Authorization where

import Telegram.Database.Api.Decoding
import Telegram.Database.Api.Channel
import Telegram.Database.Api.Utils
import Telegram.Database.Api.Messages
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts

import qualified Telegram.Database.Json as TDLib
import qualified Data.Text as Text

import Telegram.Database.Json as TDLib

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString as ByteString

import qualified Configuration.Env as Env

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

stageFour :: String -> Value
stageFour number = Object $ fromList  [
    ("@type", String "checkAuthenticationCode"),
    ("code", String $ Text.pack number)
  ]

printLoop :: Client -> IO ()
printLoop client = do
  message4 <- TDLib.receive client
  printMessage message4
  tryPrintMessage message4
  printLoop client
  where
    tryPrintMessageImpl :: (Result Message) -> IO ()
    tryPrintMessageImpl (Success msg) | canBeForwarded msg && isChannelPost msg = do
      viewMessage client msg
      forwardMessage client msg 2115507
                                      | otherwise = print msg
    tryPrintMessageImpl _ = return ()

    tryPrintMessage :: Maybe ByteString.ByteString -> IO ()
    tryPrintMessage (Just message) = tryPrintMessageImpl (getNewMessage message)
    tryPrintMessage Nothing = return ()

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

  print "!!!!!!!!!!!!!!! LOOP STARTED !!!!!!!!!!!!!!"
  printLoop client
  return client

close :: Client -> IO ()
close = TDLib.destroy

data AuthorizationState = AuthorizationStateWaitEncryptionKey | AuthorizationStateWaitPhoneNumber |
 AuthorizationStateWaitCode | AuthorizationStateReady | Unknown 
 deriving (Show, Read, Eq)

getAuthorizationStateFromString :: String -> AuthorizationState
getAuthorizationStateFromString "authorizationStateWaitEncryptionKey"   = AuthorizationStateWaitEncryptionKey
getAuthorizationStateFromString "authorizationStateWaitPhoneNumber"     = AuthorizationStateWaitPhoneNumber
getAuthorizationStateFromString "authorizationStateWaitCode"            = AuthorizationStateWaitCode
getAuthorizationStateFromString "authorizationStateReady"               = AuthorizationStateReady
getAuthorizationStateFromString _                                       = Unknown

instance FromJSON AuthorizationState where
  parseJSON = withObject "update authorization state" $ \o -> do
    type' <- o .: "@type"
    return $ getAuthorizationStateFromString type'

getAuthorizationState :: ByteString.ByteString -> Result AuthorizationState
getAuthorizationState jsonStr = if isAuthState then unpackState (getState obj) else Error "Not updateAuthorizationState"
  where
    obj = getObject jsonStr
    type' = getTypeFromObject obj
    isAuthState = isAuthStateImpl type'
    unpackState :: Result (Result AuthorizationState) -> Result AuthorizationState
    unpackState (Success result) = result
    unpackState _ = Error "Can't parse authorization_state"

    getState :: (Maybe Object) -> Result (Result AuthorizationState)
    getState (Just obj) = parse (\o -> do
      state <- o .: "authorization_state"
      return (fromJSON state))
      obj
    getState Nothing = Error "Can't parse authorization_state"

    isAuthStateImpl :: Result String -> Bool
    isAuthStateImpl (Success "updateAuthorizationState") = True
    isAuthStateImpl _ = False


-- ####################### Draft for first stage JSON #######################
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