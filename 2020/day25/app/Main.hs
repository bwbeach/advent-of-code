module Main where

import Advent (run)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = run parse part1 part2

type Problem = (Int, Int)

parse :: String -> Problem
parse =
  pairFromList . map read . words
  where
    pairFromList [a, b] = (a, b)
    pairFromList as = error ("expected list of two: " ++ show as)

part1 :: Problem -> Int
part1 (cardPublicKey, doorPublicKey) =
  encryptionKey doorPublicKey cardLoopSize
  where
    cardLoopSize = loopSize 7 cardPublicKey

part2 :: Problem -> Int
part2 _ = 0

-- | The fundamental transformation that gets repeated
transform :: Int -> Int -> Int
transform subject value = (value * subject) `mod` 20201227

-- | Find the loop size, given subject number and a public key.
loopSize :: Int -> Int -> Int
loopSize subject publicKey = fromJust . elemIndex publicKey . iterate (transform subject) $ 1

-- | Find the encryption key, given a public key of one device and loop size of the other device
encryptionKey :: Int -> Int -> Int
encryptionKey onePublicKey otherLoopSize = (!! otherLoopSize) . iterate (transform onePublicKey) $ 1
