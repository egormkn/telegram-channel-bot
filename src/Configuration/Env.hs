{-# LANGUAGE LambdaCase #-}

module Configuration.Env (load, get) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment
import Control.Monad

load :: IO ()
load = void $ loadFile defaultConfig

get :: String -> String -> IO String
get title = lookupEnv >=> \case
  Nothing    -> putStrLn ("Please, enter the " ++ title) >>= return getLine
  Just value -> return value
