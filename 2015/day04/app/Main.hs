module Main where

import Advent (run)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)

main :: IO ()
main = run parse part1 part2

type Problem = String

parse :: String -> Problem
parse = filter (/= '\n')

part1 :: Problem -> Int
part1 = solve "00000"

part2 :: Problem -> Int
part2 = solve "000000"

solve :: String -> Problem -> Int
solve prefix problem =
  head . filter ((prefix `isPrefixOf`) . hashValue) $ [1 ..]
  where
    hashValue x = md5String (problem ++ show x)

md5String :: String -> String
md5String = show . md5Digest

md5Digest :: String -> Digest MD5
md5Digest = hash . pack
