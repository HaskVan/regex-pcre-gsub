module Text.Regex.PCRE.ByteString.Gsub
  (gsub) where

import           Data.Array            ((!))
import           Data.ByteString.Char8 (ByteString)
import           Data.Char             (isDigit)
import           Text.Regex.PCRE       (MatchText, (=~))

import           Data.ByteString.Char8 as B8

-------------------------------------------------------------------------------

gsub :: ByteString -> ByteString -> ByteString -> ByteString
gsub text match replacement = replaceMatches 0 matches text
  where
    matches = (text =~ match :: [MatchText ByteString])
    rl = B8.length replacement
    replaceMatches _ [] text = text
    replaceMatches accum (m:ms) text = replaceMatches accum' ms text'
      where
      (o, l) = snd (m ! 0)
      (pre, post) = B8.splitAt (o + accum) text
      accum' = accum + (rl - l + 1)
      post' = B8.drop l post
      text' =
        B8.concat [pre, replacePlaceholder (fst $ m ! 0) match replacement, post']

      replacePlaceholder :: ByteString -> ByteString -> ByteString -> ByteString
      replacePlaceholder expr pattern sub = B8.concat $ B8.zipWith f ('_' `B8.cons` sub) sub
        where
          matches = (expr =~ pattern :: [MatchText ByteString]) !! 0
          f :: Char -> Char -> ByteString
          f _ '\\' = B8.empty
          f '\\' i
            | isDigit i = fst $ matches ! read [i]
            | otherwise = B8.pack $ '\\':[i]
          f _ x = B8.pack [x]
