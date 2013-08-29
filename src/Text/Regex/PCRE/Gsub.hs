module Text.Regex.PCRE.Gsub
  (gsub) where

import Text.Regex.PCRE ((=~),  MatchText)
import Data.Array ((!))
import Data.Char (isDigit)

-------------------------------------------------------------------------------


-- | Returns a copy of the given input string with all the occurrences of
-- regexp pattern substituted for the third argument.
--
-- If the replacement string contains back references on the form of `\\d`
-- (where `d` is a group number), they will be replaced with the captured
-- groups of the regexp match
--
-- Examples:
--
-- > gsub "hello world" "world$" "mundo" => "hello mundo"
-- > gsub "hello world " "world$" "mundo" => "hello world "
-- > gsub "hallo world" "(.*) world" "\\1-\\1" => "hallo-hallo mundo"
--
gsub :: String  -- ^ Input String
     -> String  -- ^ Regexp Pattern
     -> String  -- ^ Replacement String
     -> String  -- ^ Output String
gsub text pattern replacement = replaceMatches 0 matches text
  where
    matches = (text =~ pattern :: [MatchText String])
    rl = length replacement
    replaceMatches _ [] text = text
    replaceMatches accum (m:ms) text = replaceMatches accum' ms text'
      where
        (o, l) = snd (m ! 0)
        (pre, post) = splitAt (o + accum) text
        accum' = accum + (rl - l + 1)
        post' = drop l post
        text' = concat [ pre
                       , replacePlaceholder (fst $ m ! 0) pattern replacement
                       , post' ]

        replacePlaceholder :: String -> String -> String -> String
        replacePlaceholder expr pattern sub =
            concat $ zipWith f ('_':sub) sub
          where
            matches = (expr =~ pattern :: [MatchText String]) !! 0
            f :: Char -> Char -> String
            f '\\' i
              | isDigit i = fst $ matches ! read [i]
              | otherwise = '\\':[i]
            f _ '\\' = []
            f _ x = [x]
