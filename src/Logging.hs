{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Data.Text as T

log :: Text -> IO ()
log = print

logWithTag :: Text -> Text -> IO ()
logWithTag tag msg = print $ T.concat [tag, ": ", msg]