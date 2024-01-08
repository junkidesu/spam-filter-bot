{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Classifier
import Control.Monad.Trans.State (runState)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (Header, decodeByName)
import qualified Data.Vector as V
import Entry (Entry)

loadDataset :: FilePath -> IO (Either String (Header, V.Vector Entry))
loadDataset path = do
  file <- BL.readFile path

  return (decodeByName file :: Either String (Header, V.Vector Entry))

start :: IO ()
start = do
  dataset <- loadDataset "data/sample.csv"

  case dataset of
    Left e -> putStrLn $ "Some error occurred: " ++ e
    Right (_, entries) -> do
      let (prediction, classifier) =
            runState
              (trainOp entries >> predictOp "cash")
              emptyClassifier

      print classifier

      case prediction of
        Nothing -> putStrLn "Unknown word!"
        Just c -> print c

main :: IO ()
main = start
