module Util where

import Data.Bits
import Data.Bool

numToBits :: Bits a => a -> String
numToBits = map (bool '0' '1') . reverse . toBits
  where
    toBits b
      | b == zeroBits = []
      | otherwise = testBit b 0 : toBits (shiftR b 1)

bitsToNum :: Num a => String -> a
bitsToNum =
  sum
    . map (\(p, v) -> if v then 2 ^ p else 0)
    . zip [0 ..]
    . reverse
    . map (== '1')

lpad :: Int -> a -> [a] -> [a]
lpad m x xs = replicate (m - length ys) x ++ ys
  where
    ys = take m xs
