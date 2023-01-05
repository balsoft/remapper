{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Coords
import Data.Text (Text, intercalate, pack)
import Maps
import System.Environment
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Parsec (parse)

mkMessage :: Coords -> Text
mkMessage coords =
  intercalate
    ", "
    [ "<code>" <> renderCoords coords <> "</code>",
      "<a href=\"" <> osmandLink coords <> "\">OsmAnd</a>",
      "<a href=\"" <> osmLink coords <> "\">OSM/OrganicMaps</a>",
      "<a href=\"" <> googleLink coords <> "\">Google</a>",
      "<a href=\"" <> yandexLink coords <> "\">Yandex</a>"
    ]

usage :: Text
usage = "Try sharing a location from <b>OsmAnd</b>, <b>Organic Maps</b>, <b>Yandex.Maps</b>, or Telegram itself."

main :: IO ()
main = do
  token <- getEnv "TELEGRAM_TOKEN"
  void $ runTelegramIntegrationBot (Token (pack token)) $ \ChatChannel {..} ->
    forever $
      getUpdate channelUpdateChannel >>= \case
        SomeNewMessage (Message {messageText = Just "/start"}) ->
          void $
            sendMessage
              (sendMessageRequest channelChatId usage)
                { sendMessageParseMode = Just HTML,
                  sendMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup $ ReplyKeyboardMarkup [[KeyboardButton "Send my current location" Nothing (Just True) Nothing Nothing]] (Just True) Nothing Nothing Nothing
                }
        SomeNewMessage (Message {messageText = Just text}) -> case parse coordMessage "message" text of
          Right coords ->
            void $
              sendMessage
                (sendMessageRequest channelChatId (mkMessage coords))
                  { sendMessageParseMode = Just HTML,
                    sendMessageDisableWebPagePreview = Just True
                  }
          Left e ->
            void $
              sendMessage
                (sendMessageRequest channelChatId ("I didn't understand this message. " <> usage <> "\nTechnical details:\n<pre>" <> pack (show e) <> "</pre>"))
                  { sendMessageParseMode = Just HTML
                  }
        SomeNewMessage (Message {messageLocation = Just (Location {locationLongitude, locationLatitude})}) ->
          void $
            sendMessage
              (sendMessageRequest channelChatId (mkMessage (Coords (realToFrac locationLatitude) (realToFrac locationLongitude) Nothing)))
                { sendMessageParseMode = Just HTML,
                  sendMessageDisableWebPagePreview = Just True
                }
        _ -> pure ()
