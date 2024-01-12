{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Entry
  ( Category (Spam, Ham),
    cleanWord,
    splitMessage,
    Entry (Entry, category, content),
  )
where

import Control.Monad (MonadPlus (mzero))
import Data.Char (isAlphaNum)
import Data.Csv
  ( DefaultOrdered (headerOrder),
    Field,
    FromField (parseField),
    FromNamedRecord (parseNamedRecord),
    Header,
    NamedRecord,
    Parser,
    header,
    (.:),
  )
import Data.List (nub)
import qualified Data.Text as T

data Category = Spam | Ham
  deriving (Eq, Show)

data Entry = Entry {category :: Category, content :: T.Text}
  deriving (Show)

instance FromField Category where
  parseField :: Field -> Parser Category
  parseField s
    | s == "spam" = pure Spam
    | s == "ham" = pure Ham
    | otherwise = mzero

instance FromNamedRecord Entry where
  parseNamedRecord :: NamedRecord -> Parser Entry
  parseNamedRecord m = Entry <$> m .: "Category" <*> m .: "Message"

instance DefaultOrdered Entry where
  headerOrder :: Entry -> Header
  headerOrder _ = header ["Category", "Message"]

cleanWord :: T.Text -> T.Text
cleanWord = T.filter isAlphaNum

splitMessage :: T.Text -> [T.Text]
splitMessage = nub . map cleanWord . T.words . T.toLower
