{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

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
    counts :: WordCounts
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

type ClassifierOp = StateT Classifier IO

emptyClassifier :: Classifier
emptyClassifier =
  Classifier
    { total = (0, 0),
      counts = Map.empty
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

trainOp :: V.Vector Entry -> ClassifierOp ()
trainOp entries = V.forM_ entries addEntryOp

predictWord :: T.Text -> Classifier -> Maybe Double
predictWord word classifier =
  do
    (spamCount, hamCount) <- Map.lookup word (counts classifier)

    let prob :: Double
        prob = fromIntegral spamCount / fromIntegral (spamCount + hamCount)

    return prob

classify :: Double -> Category
classify prob
  | prob >= 0.7 = Spam
  | otherwise = Ham
