module ParallelOps (countVowelsParallel) where

import Control.Parallel.Strategies (parList, rseq, using, Eval)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

countVowelsInChunk :: T.Text -> Int
countVowelsInChunk chunk = T.foldl' count 0 chunk
  where
    count :: Int -> Char -> Int
    count acc c
      | c `elem` "aeiouAEIOU" = acc + 1
      | otherwise = acc

countVowelsParallel :: LBS.ByteString -> Int
countVowelsParallel lazyByteString =
  let textContent = decodeUtf8 (LBS.toStrict lazyByteString)
  in
    let
      chunks = T.chunksOf 10000 textContent
      counts = map countVowelsInChunk chunks
      parallelCounts = counts `using` parList rseq
    in
      sum parallelCounts