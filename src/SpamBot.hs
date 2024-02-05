{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SpamBot (run) where

import Classifier
  ( Classifier,
    classify,
    emptyClassifier,
    predictWord,
    trainOp,
  )
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.State
import qualified Data.Text as T
import Entry (loadDataset)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = Classifier

data Action = Start | ViewStats | Predict T.Text
  deriving (Eq, Show, Read)

initBot :: IO (BotApp Model Action)
initBot = do
  res <- liftIO $ loadDataset "data/sample.csv"

  case res of
    Right entries -> do
      currentClassifier <- execStateT (trainOp entries) emptyClassifier

      return
        BotApp
          { botInitialModel = currentClassifier,
            botAction = flip updateToAction,
            botHandler = handleAction,
            botJobs = []
          }
    Left _ ->
      return $
        BotApp
          { botInitialModel = emptyClassifier,
            botAction = flip updateToAction,
            botHandler = handleAction,
            botJobs = []
          }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $
    Start <$ command "start"
      <|> ViewStats <$ command "view_stats"
      <|> Predict <$> command "predict"
      <|> callbackQueryDataRead

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Start -> do
    model <# do
      replyText $
        T.unlines
          [ "Welcome to Naive Bayes Spam Filter bot!",
            "",
            "To see classifier stats, try /view_stats",
            "To check a word for spam, try /predict <word>"
          ]
  ViewStats ->
    model <# do
      replyText . T.pack . show $ model
  Predict "" ->
    model <# do
      replyText "No word was given!"
  Predict word ->
    model <# do
      case predictWord word model of
        Nothing -> replyText "Word not found!"
        Just prob -> do
          replyText $
            T.unlines
              [ "Probability of spam: " <> (T.pack . show $ prob),
                "Predicted class: " <> (T.pack . show . classify $ prob)
              ]

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (conversationBot updateChatId bot) env