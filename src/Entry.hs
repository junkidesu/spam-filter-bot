{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Entry
  ( Category (Spam, Ham),
    cleanWord,
    splitMessage,
    Entry (Entry, category, content),
    loadDataset,
  )
where

import Control.Monad (MonadPlus (mzero))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Csv
  ( DefaultOrdered (headerOrder),
    Field,
    FromField (parseField),
    FromNamedRecord (parseNamedRecord),
    Header,
    NamedRecord,
    Parser,
    decodeByName,
    header,
    (.:),
  )
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Vector as V

data Category = Spam | Ham
  deriving (Eq, Show, Read)

data Entry = Entry {category :: Category, content :: T.Text}
  deriving (Show)

instance FromField Category where
  parseField :: Field -> Parser Category
  parseField "spam" = pure Spam
  parseField "ham" = pure Ham
  parseField _ = mzero

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

loadDataset :: FilePath -> IO (Either String (V.Vector Entry))
loadDataset path = do
  file <- BL.readFile path

  let res = decodeByName file :: Either String (Header, V.Vector Entry)

  case res of
    Left err -> return . Left $ err
    Right (_, entries) -> return . Right $ entries