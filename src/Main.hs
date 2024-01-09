{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Classifier
import Control.Monad.Trans.State (runState)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (Header, decodeByName)
import qualified Data.Foldable as DF
import qualified Data.Text as T
import qualified Data.Vector as V
import Entry (Entry)

loadDataset :: FilePath -> IO (Either String (Header, V.Vector Entry))
loadDataset path = do
  file <- BL.readFile path

  return (decodeByName file :: Either String (Header, V.Vector Entry))

train :: IO (Maybe Classifier)
train = do
  putStrLn "Loading dataset..."

  dataset <- loadDataset "data/sample.csv"

  case dataset of
    Left e -> do
      putStrLn $ "Some error occurred: " ++ e
      return Nothing
    Right (_, entries) -> do
      putStrLn "Dataset loaded! Starting training..."

      let (_, classifier) =
            runState
              (trainOp entries)
              emptyClassifier

      putStrLn "Training complete!"

      return (pure classifier)

interpreter :: Classifier -> IO ()
interpreter classifier = do
  putStrLn ""
  putStrLn "Commands: "
  putStrLn "(p)redict"
  putStrLn "(q)uit"
  putStrLn ""

  putStr "? "
  command <- getLine

  case command of
    "q" -> return ()
    "p" -> do
      putStr "Give your word: "

      word <- getLine

      let p = predictWord (T.pack word) classifier

      case p of
        Nothing -> do
          putStrLn "Word not found!"
          interpreter classifier
        Just prob -> do
          putStrLn ("Probability: " ++ show prob)
          putStrLn ("Predicted category: " ++ show (classify 0.8 prob))

      interpreter classifier
    _ -> do
      putStrLn "Invalid command"
      interpreter classifier

main :: IO ()
main = do
  c <- train

  DF.forM_ c interpreter