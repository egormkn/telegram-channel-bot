{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.MessageParser where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Read            (readMaybe)

import           Telegram.Bot.API

newtype MessageParser a = MessageParser
  { runMessageParser :: Text -> Maybe a
  } deriving (Functor)

instance Applicative MessageParser where
  pure x = MessageParser (pure (pure x))
  MessageParser f <*> MessageParser x = MessageParser (\u -> f u <*> x u)

instance Alternative MessageParser where
  empty = MessageParser (const Nothing)
  MessageParser f <|> MessageParser g = MessageParser (\u -> f u <|> g u)

instance Monad MessageParser where
  return = pure
  MessageParser x >>= f = MessageParser (\u -> x u >>= flip runMessageParser u . f)
  fail _ = empty

mkParser :: (Text -> Maybe a) -> MessageParser a
mkParser = MessageParser

parseMessage :: MessageParser a -> Text -> Maybe a
parseMessage = runMessageParser

text :: MessageParser Text
text = MessageParser Just

plainText :: MessageParser Text
plainText = do
  t <- text
  if "/" `Text.isPrefixOf` t
    then fail "command"
    else pure t

command :: Text -> MessageParser Text
command name = do
  t <- text
  case Text.words t of
    (w:ws) | w == "/" <> name
      -> pure (Text.unwords ws)
    _ -> fail "not that command"
