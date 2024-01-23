{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib where

import Classifier
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BL
import Data.Csv (Header, decodeByName)
import qualified Data.Text as T
import qualified Data.Vector as V
import Entry (Entry)

loadDataset :: FilePath -> IO (Either String (V.Vector Entry))
loadDataset path = do
  file <- BL.readFile path

  let res = decodeByName file :: Either String (Header, V.Vector Entry)

  case res of
    Left e -> return . Left $ e
    Right (_, es) -> return . Right $ es

train :: ClassifierOp ()
train = do
  liftIO $ putStrLn "Loading dataset..."

  dataset <- liftIO $ loadDataset "data/sample.csv"

  case dataset of
    Left e -> do
      liftIO $ putStrLn $ "Error occured: " ++ e
    Right es -> do
      trainOp es
      liftIO $ putStrLn "Training complete!"

interpreter :: ClassifierOp ()
interpreter = do
  command <- liftIO $ do
    putStrLn "\nCommands:"
    putStrLn "(q)uit"
    putStrLn "(p)redict\n"
    putStr "> "
    getLine

  case command of
    "q" -> do
      liftIO $ putStrLn "Bye!"
    "p" -> do
      word <- liftIO $ do
        putStr "Enter your word: "
        getLine

      mp <- predictOp . T.toLower . T.pack $ word

      case mp of
        Nothing -> do
          liftIO $ putStrLn "Word not found!"
        Just p -> do
          liftIO $ do
            putStrLn $ "Probability: " ++ show p
            putStrLn $ "Predicted class: " ++ show (classify 0.7 p)

      interpreter
    _ -> do
      liftIO $ putStrLn "Invalid command"
      interpreter

start :: IO ()
start = evalStateT (train >> interpreter) emptyClassifier