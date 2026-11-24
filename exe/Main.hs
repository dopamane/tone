module Main (main) where

import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Rap
import Data.Version
import Data.Word
import Options.Applicative
import Paths_tone
import Sound.Tone

main :: IO ()
main = do
  (t, a, sr, p, f) <- customExecParser prefs' $
    info (parser <**> simpleVersioner (showVersion version) <**> helper)
    (progDesc "tone synthesizer")
  when (a < 0 || a > 100) $ fail $ "amp percent out of range: " ++ show a
  when (sr <= 0) $ fail $ "sample rate not positive: " ++ show sr
  when (p < 0 || p >= 2 * pi) $ fail $ "phase offset rads out of range: " ++ show p
  when (f < 0 || f >= fromIntegral sr / 2) $ fail $ "frequency out of range: " ++ show f
  let smpCnt = round (fromIntegral sr * t) :: Integer
  when (smpCnt > fromIntegral (maxBound :: Word32)) $
    fail $ "sample count out of range: " ++ show smpCnt
  let sound = tone (fromIntegral sr) (a / 100) (p / (2 * pi)) f
  BS.putStr $ runPut $ putRap $ wav sr $ take (fromIntegral smpCnt) sound

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

parser :: Parser (Double, Double, Word32, Double, Double)
parser = (,,,,)
  <$> option auto (short 't' <> long "time" <> metavar "DBL" <> value 1
      <> showDefault <> help "duration sec")
  <*> option auto (short 'a' <> long "amp" <> metavar "DBL" <> value 80
      <> showDefault <> help "amplitude percent")
  <*> option auto (short 's' <> long "samp" <> metavar "INT" <> value 44100
      <> showDefault <> help "sample rate")
  <*> option auto (short 'p' <> long "phase" <> metavar "DBL" <> value 0
      <> showDefault <> help "phase offset rads")
  <*> argument auto (metavar "DBL" <> help "frequency Hz")

wav :: Word32 -> Tone -> Rap
wav sr sound = [Stanza "RIFF" "WAVE"
  [ Verse "fmt " $ runPut $ do
    putWord16le 0x0001
    putWord16le 1
    putWord32le sr
    putWord32le $ sr * 2
    putWord16le 2
    putWord16le 16
    putWord16le 0
  , Verse "data" $ runPut $ foldMap (putInt16le . denorm) sound]]

denorm :: Double -> Int16
denorm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ negate $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
