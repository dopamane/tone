module Sound.Tone (Tone, tone) where

type Tone = [Double]

tone
  :: Double -- ^ sample rate
  -> Double -- ^ amplitude [0,1]
  -> Double -- ^ offset [0,1)
  -> Double -- ^ frequency
  -> Tone
tone sr a o f = [a * sin (2 * pi * phase) | phase <- iterate acc o]
  where
    acc phi | phi' >= 1 = phi' - 1
            | otherwise = phi'
      where
        phi' = phi + f / sr
