module Regex where

import Text.Regex.Posix (getAllMatches, (=~), AllMatches, MatchOffset, MatchLength)

-------------------------------------------------------------------------------
gsub :: String -> String -> String -> String
gsub text match replacement = replaceMatches 0 matches text
  where
    matches = getAllMatches 
      (text =~ match :: AllMatches [] (MatchOffset, MatchLength))
    rl = length replacement
    replaceMatches _ [] text = text
    replaceMatches accum ((o, l):ms) text = replaceMatches accum' ms text'
      where
      (pre, post) = splitAt (o + accum) text
      accum' = accum + (rl - l)
      post' = drop l post
      text' = pre ++ replacement ++ post'

main :: IO ()
main = print (gsub "roman andre gonzalez urdaneta" "[ng]" "_")

