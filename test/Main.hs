module Main (main) where

import Sound.Tone
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Sound.Tone"
  [testCase "100 samples" $ do
    let sound = tone 48000 1 0 480
    sound !! 0 @?= 0
    sound !! 25 @?= 1
    rnd12p (sound !! 50) @?= 0
    sound !! 75 @?= (-1)
  , testCase "4 samples" $ do
    let sound = tone 48000 1 0 12000
    (map rnd12p . take 4) sound @?= [0, 1, 0, -1]
  , testCase "phase" $ do
    let sound = tone 48000 1 ((pi / 2) / (2 * pi)) 12000
    (map rnd12p . take 4) sound @?= [1, 0, -1, 0]
  ]

rnd12p :: Double -> Double
rnd12p x = fromIntegral (round (x * 1e12) :: Integer) / 1e12
