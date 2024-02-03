{-# LANGUAGE OverloadedStrings #-}

module Lib (start) where

import qualified Data.Text as T
import SpamBot (run)
import Telegram.Bot.API (Token (Token))

start :: IO ()
start = do
  putStrLn "Please, enter Telegram bot's API token:"
  getLine >>= run . Token . T.pack