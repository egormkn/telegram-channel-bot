{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Authorization where

import Telegram.Database.Api.Decoding
import Telegram.Database.Api.Utils
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Data.Text

import Telegram.Database.Json as TDLib

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
        ("api_hash", String $ pack hash),
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

stageThree :: Text -> Value
stageThree number = Object $ fromList  [
    ("@type", String "setAuthenticationPhoneNumber"),
    ("phone_number", String number)
  ]

stageFour :: Text -> Value
stageFour number = Object $ fromList  [
    ("@type", String "checkAuthenticationCode"),
    ("code", String number)
  ]

waitForType :: Client -> AuthorizationState -> IO ()
waitForType client expectedState = do
  message <- receive client
  print ("WAIT: Wating for " ++ show expectedState)
  let state = getAuthStateFromMessage message
  printMessage message
  helper state
  where
    helper :: Result AuthorizationState -> IO ()
    helper (Success state) = do
      print ("########## Type recieved " ++ show state ++ " ##########")
      processState client state
    helper _ = waitForType client expectedState

    getAuthStateFromMessage :: Maybe ByteString.ByteString -> Result AuthorizationState
    getAuthStateFromMessage (Just str) = getAuthorizationState str
    getAuthStateFromMessage Nothing = Error "Can't parse auth state"

processState :: Client -> AuthorizationState -> IO ()
processState client AuthorizationStateWaitEncryptionKey = do
  send client $ encodeValue stageTwo
  waitForType client AuthorizationStateWaitPhoneNumber
processState client AuthorizationStateWaitPhoneNumber = do
  number <- Env.get "mobile phone number" "PHONE_NUMBER"
  send client $ encodeValue $ stageThree $ pack number
  waitForType client AuthorizationStateWaitCode
processState client AuthorizationStateWaitCode = do
  putStrLn "Please, enter code:"
  code <- getLine
  send client $ encodeValue $ stageFour $ pack code
  waitForType client AuthorizationStateReady
processState _ AuthorizationStateReady = return ()
processState client _ = waitForType client AuthorizationStateReady

authorize :: ApiKey -> IO Client
authorize key = do
  client <- TDLib.create
  send client "{\"@type\": \"getAuthorizationState\", \"@extra\": 1.01234}"
  send client $ encodeValue $ stageOne key
  waitForType client AuthorizationStateWaitEncryptionKey
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
getAuthorizationState jsonStr = if isAuthState then unpackState (getState maybeObj) else Error "Not updateAuthorizationState"
  where
    maybeObj = getObject jsonStr
    type' = getTypeFromObject maybeObj
    isAuthState = isAuthStateImpl type'
    unpackState :: Result (Result AuthorizationState) -> Result AuthorizationState
    unpackState (Success result) = result
    unpackState _ = Error "Can't parse authorization_state"

    getState :: Maybe Object -> Result (Result AuthorizationState)
    getState (Just obj) = parse (\o -> do
      state <- o .: "authorization_state"
      return (fromJSON state))
      obj
    getState Nothing = Error "Can't parse authorization_state"

    isAuthStateImpl :: Result Text -> Bool
    isAuthStateImpl (Success "updateAuthorizationState") = True
    isAuthStateImpl _ = False
