{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Maps where

import Coords
import Data.Text

-- lon, lat for some reason
yandexLink :: Coords -> Text
yandexLink Coords {..} = "https://yandex.com/maps?pt=" <> pack (show lon) <> "," <> pack (show lat) <> "&z=" <> showZoom zoom <> "&l=map"

googleLink :: Coords -> Text
googleLink coords = "https://www.google.com/maps/search/?api=1&query=" <> renderCoords coords

osmandLink :: Coords -> Text
osmandLink Coords {..} = "https://osmand.net/go/?lat=" <> pack (show lat) <> "&lon=" <> pack (show lon) <> "&z=" <> showZoom zoom

osmLink :: Coords -> Text
osmLink Coords {..} = "https://openstreetmap.org/?mlat=" <> pack (show lat) <> "&mlon=" <> pack (show lon) <> "#map=" <> showZoom zoom <> "/" <> pack (show lat) <> "/" <> pack (show lon)
