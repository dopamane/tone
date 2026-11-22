module Sound.Tone (tone) where

import Data.Binary.Put
import Data.Int
import Data.Rap
import Data.Word

tone
  :: Double -- ^ duration sec
  -> Double -- ^ amplitude pct
  -> Word32 -- ^ sample rate
  -> Double -- ^ freq Hz
  -> Rap
tone t a sr f = [Stanza "RIFF" "WAVE"
  [ Verse "fmt " $ runPut $ do
    putWord16le 0x0001
    putWord16le 1
    putWord32le sr
    putWord32le $ sr * 2
    putWord16le 2
    putWord16le 16
    putWord16le 0
  , Verse "data" $ runPut $ foldMap (putInt16le . norm)
    [ (a / 100) * sin (2 * pi * f * fromIntegral i / fromIntegral sr)
    | i <- [0 :: Integer .. round (fromIntegral sr * t) - 1]]
  ]]

norm :: Double -> Int16
norm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
