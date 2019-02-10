{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Telegram.Bot.Parser where


import           Control.Applicative
import           Control.Monad.Reader
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text

newtype Parser a = Parser { 
  runParser :: Text -> Maybe (a, Text)
} deriving (Functor)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser fs <*> Parser sn = Parser mb where
      mb = fs >=> \(f', s) -> sn s 
              >>= \(x, s') -> Just (f' x, s')

instance Alternative Parser where
  empty :: Parser a  -- always fails
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a  -- run first, if fails — run second
  Parser a <|> Parser b = Parser $ \u -> a u <|> b u

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser x >>= f = Parser $ x >=> \(ta, s) -> runParser (f ta) s

mkParser :: (Text -> Maybe (a, Text)) -> Parser a
mkParser = Parser

parseCommand :: Parser a -> Text -> Maybe (a, Text)
parseCommand = runParser

textParser :: Parser Text
textParser = Parser (\s -> Just (s, ""))

plainText :: Parser Text
plainText = do
  t <- textParser
  if "/" `Text.isPrefixOf` t
  then fail "command"
  else pure t

command :: Text -> Parser Text
command name = do
  t <- textParser
  case Text.words t of
    (w:ws) | w == "/" <> name -> pure (Text.unwords ws)
    _                         -> fail "not that command"
