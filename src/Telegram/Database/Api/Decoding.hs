{-# LANGUAGE OverloadedStrings #-}

module Telegram.Database.Api.Decoding (
  AuthorizationState(..),
  getField,
  getType,
  getAuthorizationState
  ) where

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

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

getType :: ByteString.ByteString -> Result String
getType = getField "@type"

getTypeFromObject :: Maybe Object -> Result String
getTypeFromObject = getFieldFromObject "@type"

getField :: Text.Text -> ByteString.ByteString -> Result String
getField fieldName str = getFieldFromObject fieldName $ getObject str

getFieldFromObject :: Text.Text -> Maybe Object -> Result String
getFieldFromObject _ Nothing = Error "can't parse"
getFieldFromObject fieldName (Just obj) = parse helper obj
  where
    helper :: Object -> Parser String
    helper o = o .: fieldName

getObject :: ByteString.ByteString -> Maybe Object
getObject str = decodeStrict str :: Maybe Object