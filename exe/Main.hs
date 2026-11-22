module Main (main) where

import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Rap
import Data.Version
import Data.Word
import Options.Applicative
import Paths_tonwav
import Sound.Tone

main :: IO ()
main = do
  CLI t a sr f <- customExecParser prefs' $ pinfo $ showVersion version
  when (a < 0 || a > 100) $ fail $ "amp percent out of range: " ++ show a
  BS.putStr $ runPut $ putRap $ tone t a sr f

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
