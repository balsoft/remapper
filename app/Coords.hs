{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Coords where

import Control.Monad (replicateM, void)
import Data.Bits
import Data.List (elemIndex)
import Data.Text hiding (drop, foldr, groupBy, reverse, splitAt, tail, unpack)
import Data.Word (Word8)
import Text.Parsec
import Text.Parsec.Text
import Text.Printf
import Data.Maybe (fromMaybe)

data Coords = Coords {lat :: Float, lon :: Float, zoom :: Maybe Float} deriving (Eq)

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

instance Show Coords where
  show (Coords {..}) = roundToStr 5 lat <> "," <> roundToStr 5 lon

renderCoords :: Coords -> Text
renderCoords = Data.Text.pack . show

showZoom :: Maybe Float -> Text
showZoom = pack . show . round . fromMaybe 17.0

parseFloat :: Parser Float
parseFloat = read <$> parser
  where
    parser = mappend <$> number <*> option "" ((:) <$> char '.' <*> number)
    number = many1 digit

coordMessage :: Parser Coords
coordMessage = (try coords <|> try geoLink <|> try osmandShare <|> try ge0Part <|> try omLink <|> try omShare <|> try yandexShare) <* eof
  where
    coords = Coords <$> parseFloat <* char ',' <* many (char ' ') <*> parseFloat <*> pure Nothing

    geoLink = do
      Coords { lat, lon } <- coords
      void $ string "?z="
      zoom <- Just <$> parseFloat
      void $ many (noneOf " \n\t")
      pure Coords {..}

    osmandShare = manyTill anyChar (try $ string "geo:") *> geoLink <* char '\n' <* many anyChar

    ge0Part = ge0 <$> replicateM 10 (oneOf base64Alphabet)

    omLink = string "om://" *> ge0Part <* optional (char '/' <* many (oneOf base64Alphabet))

    omShare = manyTill anyChar (try (string "om://")) *> ge0Part <* many anyChar

    yandexShare = do
      void $ manyTill anyChar (try (string "https://yandex.com/maps"))
      void $ string "?whatshere%5Bpoint%5D="
      lon <- parseFloat
      void $ string "%2C"
      lat <- parseFloat
      void $ string "&whatshere%5Bzoom%5D="
      zoom <- Just <$> parseFloat
      void $ many anyChar
      pure Coords {..}

fromListBE :: [Bool] -> Int
fromListBE = foldr f 0 . reverse
  where
    fromBool True = bit 0
    fromBool False = 0
    f b i = fromBool b .|. (i `shiftL` 1)

toListBE :: (FiniteBits b) => b -> [Bool]
toListBE b = testBit b <$> reverse [0 .. finiteBitSize b - 1]

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = let (a, xs') = splitAt n xs in a : groupBy n xs'

padL :: Int -> a -> [a] -> [a]
padL n x xs = xs <> (x <$ [0 .. n])

base64Alphabet :: String
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

base64ToBits :: Char -> Maybe (Bool, Bool, Bool, Bool, Bool, Bool)
base64ToBits ch = case toListBE <$> i of
  Just [False, False, a, b, c, d, e, f] -> Just (a, b, c, d, e, f)
  _ -> Nothing
  where
    i :: Maybe Word8
    i = fromIntegral <$> elemIndex ch base64Alphabet

-- WTF WTF WTF
-- ge0 is a base64-encoded bytestring of characters c, no longer than 11 characters total:
-- cccccccccc

-- ^           zoom level
--  ^^^^^^^^^  latitude and longtitude
-- For some reason, to get the actual zoom level, you have to do (c / 4) + 4 to the zoom level byte.
-- But it gets worse. Latitude and longtitude information is bit-interleaved in every base64 character!!!
-- bbbbbb
-- ^ ^ ^  latitude (3 bits)
--  ^ ^ ^ longtitiude (3 bits)
-- But ooooh it gets worse. The last 3 bits are assumed to be 000, and then we get a number between 0 and 2^30
-- We then divide it by 2^30, multiply by maximum possible value (180 for lat, 360 for lon) and subtract half that (90 for lat, 180 for lon)
-- Whoever came up with this shit must hate sane programmers.

ge0 :: String -> Coords
--  bs stands for bullshit
ge0 bs = Coords {..}
  where
    maxBytes = 10
    maxBits = maxBytes * 3

    maxInt :: Int
    maxInt = bit maxBits

    zoom' = fromListBE
        $ ( \case
              (Just (a, b, c, d, e, f)) -> [a, b, c, d, e, f]
              _ -> [False]
          )
        $ base64ToBits
        $ Prelude.head bs

    bits6 = Prelude.drop 1 $ base64ToBits <$> bs
    (latBits3, lonBits3) =
      unzip
        ( ( \case
              Just (lat1, lon1, lat2, lon2, lat3, lon3) -> ([lat1, lat2, lat3], [lon1, lon2, lon3])
              _ -> ([True, True, True], [True, True, True])
          )
            <$> bits6
        )

    latBits6 = mconcat latBits3
    lonBits6 = mconcat lonBits3

    middleOfSquare = bit (3 * (maxBytes - (Prelude.length bs - 1)) - 1)
    padding = Prelude.replicate ((maxBytes - Prelude.length bs + 1) * 3) False

    lat' :: Int
    lat' = fromListBE (latBits6 <> padding) + middleOfSquare
    lon' :: Int
    lon' = fromListBE (lonBits6 <> padding) + middleOfSquare

    zoom = Just $ fromIntegral zoom' / 4.0 + 4.0
    lat = realToFrac $ 180 * (fromIntegral lat' :: Double) / fromIntegral maxInt - 90
    lon = realToFrac $ 360 * (fromIntegral lon' :: Double) / fromIntegral maxInt - 180
