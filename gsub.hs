module Regex where

import Text.Regex.Posix (getAllMatches, (=~), AllMatches, MatchOffset, MatchLength, MatchText)

-- import Text.Regex.Posix (getAllMatches, (=~), AllMatches, MatchOffset, MatchLength, MatchText)
import Data.Array ((!))
import Data.Char (isDigit)

-------------------------------------------------------------------------------
gsub :: String -> String -> String -> String
gsub text match replacement = replaceMatches 0 matches text
  where
    matches = 
      (text =~ match :: [MatchText String])
    rl = length replacement
    replaceMatches _ [] text = text
    replaceMatches accum (m:ms) text = replaceMatches accum' ms text'
      where
      (o, l) = snd (m ! 0)
      (pre, post) = splitAt (o + accum) text
      accum' = accum + (rl - l)
      post' = drop l post
      text' = pre ++ replacePlaceholder (fst $ m ! 0) match replacement ++ post'

      replacePlaceholder :: String -> String -> String -> String
      replacePlaceholder expr pattern sub = concat $ zipWith f ('_':sub) sub
        where
          matches = (expr =~ pattern :: [MatchText String]) !! 0
          f :: Char -> Char -> String
          f i '\\' = []
          f '\\' i
            | isDigit i = fst $ matches ! read [i]
            | otherwise = '\\':[i]
          f _ x = [x]

main :: IO ()
main = print (gsub "roman andre gonzalez urdaneta" "a(.)" "[\\0 \\1]")

