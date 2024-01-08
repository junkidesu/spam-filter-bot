{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE InstanceSigs #-}

module Classifier where

import Control.Monad (forM_)
import Control.Monad.Trans.State (State, get, put)
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
  show (Classifier (s, h) wcs)= "Naive Bayes Spam Classifier\n"
    ++ "total spam messages: " ++ show s ++ "\n"
    ++ "total ham messages: " ++ show h ++ "\n"
    ++ "words in the classifier: " ++ show (Map.size wcs)


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

addWordOp :: Category -> T.Text -> State Classifier ()
addWordOp c w = do
  (Classifier t wcs) <- get
  put $ Classifier t (updateWordCounts c w wcs)

addEntryOp :: Entry -> State Classifier ()
addEntryOp (Entry cat cont) =
  let ws = splitMessage cont
   in do
        forM_ ws (addWordOp cat)
        (Classifier t wcs) <- get
        put $ Classifier (updateTotals cat t) wcs

trainOp :: V.Vector Entry -> State Classifier ()
trainOp es = V.forM_ es addEntryOp

predictOp :: T.Text -> State Classifier (Maybe Bool)
predictOp w = predictWord w <$> get
  where
    predictWord :: T.Text -> Classifier -> Maybe Bool
    predictWord word (Classifier (spamTotal, hamTotal) wcs) =
      do
        (spamCount, hamCount) <- Map.lookup word wcs

        let p :: Double
            p = fromIntegral spamCount / fromIntegral spamTotal
            q :: Double
            q = fromIntegral hamCount / fromIntegral hamTotal
            r :: Double
            r = p / (p + q)

        return (r >= 0.9)
