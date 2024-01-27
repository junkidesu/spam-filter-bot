{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Classifier where

import Control.Monad (forM_)
import Control.Monad.Trans.State (StateT, get, put)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Entry (Category (..), Entry (..), splitMessage)

type WordCounts = Map.Map T.Text (Int, Int)

data Classifier = Classifier
  { total :: (Int, Int),
    counts :: WordCounts,
    threshold :: Double
  }

instance Show Classifier where
  show :: Classifier -> String
  show classifier =
    "Naive Bayes Spam Classifier\n"
      ++ "total spam messages: "
      ++ (show . fst . total $ classifier)
      ++ "\n"
      ++ "total ham messages: "
      ++ (show . snd . total $ classifier)
      ++ "\n"
      ++ "words in the classifier: "
      ++ (show . Map.size . counts $ classifier)
      ++ "\n"
      ++ "prediction threshold: "
      ++ (show . threshold $ classifier)

type ClassifierOp = StateT Classifier IO

emptyClassifier :: Double -> Classifier
emptyClassifier threshold =
  Classifier
    { total = (0, 0),
      counts = Map.empty,
      threshold = threshold
    }

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)

updateTotals :: Category -> (Int, Int) -> (Int, Int)
updateTotals Spam (s, h) = (s + 1, h)
updateTotals Ham (s, h) = (s, h + 1)

updateWordCounts :: Category -> T.Text -> WordCounts -> WordCounts
updateWordCounts Spam word = Map.insertWith addTuples word (1, 0)
updateWordCounts Ham word = Map.insertWith addTuples word (0, 1)

addWordOp :: Category -> T.Text -> ClassifierOp ()
addWordOp category word = do
  classifier <- get
  put $
    Classifier
      (total classifier)
      (updateWordCounts category word . counts $ classifier)
      (threshold classifier)

addEntryOp :: Entry -> ClassifierOp ()
addEntryOp entry =
  let words = splitMessage . content $ entry
   in do
        forM_ words (addWordOp . category $ entry)
        classifier <- get
        put $
          Classifier
            (updateTotals (category entry) (total classifier))
            (counts classifier)
            (threshold classifier)

trainOp :: V.Vector Entry -> ClassifierOp ()
trainOp entries = V.forM_ entries addEntryOp

predictOp :: T.Text -> StateT Classifier IO (Maybe Double)
predictOp word = predictWord word <$> get

predictWord :: T.Text -> Classifier -> Maybe Double
predictWord word classifier =
  do
    (spamCount, hamCount) <- Map.lookup word (counts classifier)

    let prob :: Double
        prob = fromIntegral spamCount / fromIntegral (spamCount + hamCount)

    return prob

classify :: Double -> Double -> Category
classify threshold prob
  | prob >= threshold = Spam
  | otherwise = Ham
