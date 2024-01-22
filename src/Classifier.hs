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
import Entry (Category (..), Entry (Entry), splitMessage)

type WordCounts = Map.Map T.Text (Int, Int)

data Classifier = Classifier
  { total :: (Int, Int),
    counts :: WordCounts
  }

instance Show Classifier where
  show :: Classifier -> String
  show (Classifier (s, h) wcs) =
    "Naive Bayes Spam Classifier\n"
      ++ "total spam messages: "
      ++ show s
      ++ "\n"
      ++ "total ham messages: "
      ++ show h
      ++ "\n"
      ++ "words in the classifier: "
      ++ show (Map.size wcs)

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
updateWordCounts Spam w = Map.insertWith addTuples w (1, 0)
updateWordCounts Ham w = Map.insertWith addTuples w (0, 1)

addWordOp :: Category -> T.Text -> ClassifierOp ()
addWordOp c w = do
  (Classifier t wcs) <- get
  put $ Classifier t (updateWordCounts c w wcs)

addEntryOp :: Entry -> ClassifierOp ()
addEntryOp (Entry cat cont) =
  let ws = splitMessage cont
   in do
        forM_ ws (addWordOp cat)
        (Classifier t wcs) <- get
        put $ Classifier (updateTotals cat t) wcs

trainOp :: V.Vector Entry -> ClassifierOp ()
trainOp es = V.forM_ es addEntryOp

predictOp :: T.Text -> StateT Classifier IO (Maybe Double)
predictOp w = predictWord w <$> get

predictWord :: T.Text -> Classifier -> Maybe Double
predictWord word (Classifier _ wcs) =
  do
    (spamCount, hamCount) <- Map.lookup word wcs

    let prob :: Double
        prob = fromIntegral spamCount / fromIntegral (spamCount + hamCount)

    return prob

classify :: Double -> Double -> Category
classify threshold prob
  | prob >= threshold = Spam
  | otherwise = Ham
