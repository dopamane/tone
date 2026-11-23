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
  CLI t a sr f <- customExecParser prefs' $ pinfo $ showVersion version
  when (a < 0 || a > 100) $ fail $ "amp percent out of range: " ++ show a
  BS.putStr $ runPut $ putRap $ wav t a sr f

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "tone wav synthesizer"

data CLI = CLI Double Double Word32 Double

parser :: Parser CLI
parser = CLI
  <$> option auto (short 't' <> long "time" <> metavar "DBL" <> value 1
      <> showDefault <> help "duration sec")
  <*> option auto (short 'a' <> long "amp" <> metavar "DBL" <> value 80
      <> showDefault <> help "amplitude percent")
  <*> option auto (short 's' <> long "samp" <> metavar "INT" <> value 44100
      <> showDefault <> help "sample rate")
  <*> argument auto (metavar "DBL" <> help "frequency Hz")

wav
  :: Double -- ^ duration sec
  -> Double -- ^ amplitude pct
  -> Word32 -- ^ sample rate
  -> Double -- ^ freq Hz
  -> Rap
wav t a sr f = [Stanza "RIFF" "WAVE"
  [ Verse "fmt " $ runPut $ do
    putWord16le 0x0001
    putWord16le 1
    putWord32le sr
    putWord32le $ sr * 2
    putWord16le 2
    putWord16le 16
    putWord16le 0
  , Verse "data" $ runPut $ foldMap (putInt16le . norm) $ take numSamps $
    tone (fromIntegral sr) (a / 100) 0 f]]
  where
    numSamps = round $ fromIntegral sr * t

norm :: Double -> Int16
norm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
