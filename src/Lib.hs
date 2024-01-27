{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
    Left err -> return . Left $ err
    Right (_, entries) -> return . Right $ entries

train :: ClassifierOp ()
train = do
  liftIO $ putStrLn "Loading dataset..."

  dataset <- liftIO $ loadDataset "data/sample.csv"

  case dataset of
    Left err -> do
      liftIO $ putStrLn $ "Error occured: " ++ err
    Right entries -> do
      trainOp entries
      liftIO $ putStrLn "Training complete!"

interpreter :: ClassifierOp ()
interpreter = do
  command <- liftIO $ do
    putStrLn "\nCommands:"
    putStrLn "(q)uit"
    putStrLn "(s)tats"
    putStrLn "(p)redict\n"
    putStr "> "
    getLine

  case command of
    "q" -> do
      liftIO $ putStrLn "Bye!"
    "s" -> do
      classifier <- get
      liftIO $ print classifier
      interpreter
    "p" -> do
      word <- liftIO $ do
        putStr "Enter your word: "
        getLine

      maybeProb <- predictOp . T.toLower . T.pack $ word

      case maybeProb of
        Nothing -> do
          liftIO $ putStrLn "Word not found!"
        Just prob -> do
          classifier <- get
          liftIO $ do
            putStrLn $ "Probability: " ++ show prob
            putStrLn $
              "Predicted class: "
                ++ ( show
                       . classify (threshold classifier)
                       $ prob
                   )

      interpreter
    _ -> do
      liftIO $ putStrLn "Invalid command"
      interpreter

start :: IO ()
start = do
  putStr "Enter prediction threshold: "
  threshold <- readLn :: IO Double

  evalStateT
    (train >> interpreter)
    (emptyClassifier threshold)