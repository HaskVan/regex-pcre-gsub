{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 (ByteString)
import           Test.Hspec            (Spec, describe, hspec, it)
import           Test.HUnit            (assertBool, assertEqual)
import           Text.Regex.PCRE       ((=~))

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Text.Regex.PCRE.Gsub as G
import qualified Text.Regex.PCRE.ByteString.Gsub as BG

gsubS :: String -> String -> String -> String -> String -> Spec
gsubS desc input regex replacement expectedOutput
  = it desc $
    assertEqual "should replace correctly" expectedOutput (G.gsub input regex replacement)

gsubBS :: String -> ByteString -> ByteString -> ByteString -> ByteString -> Spec
gsubBS desc input regex replacement expectedOutput
  = it desc $
    assertEqual "should replace correctly" expectedOutput (BG.gsub input regex replacement)


main :: IO ()
main = hspec $ do
  describe "gsub string" $ do

    gsubS "simple match" "hola mundo" "mundo" "world" "hola world"
    gsubS "medium match" "hola munda" "mund[oa]" "world" "hola world"
    gsubS "hard match" "hola mundo" "(.*) mundo" "" ""
    gsubS "fucking hard match" "hola munda, hola mundo." "ho([la]+) mu([ndoa]+)" "\\1 \\2" "la nda, la ndo."
    gsubS "tricky match" "\"hello\"" "hello" "hola" "\"hola\""
    gsubS "replacement"  "hola mundo" "(.*) mundo" "\\1" "hola"
    gsubS "double replacement" "hola mundo como estas" "(.*) mundo" "\\1-\\1" "hola-hola como estas"

  describe "gsub bytestring" $ do
    gsubBS "simple match" "hola mundo" "mundo" "world" "hola world"
    gsubBS "medium match" "hola munda" "mund[oa]" "world" "hola world"
    gsubBS "hard match" "hola mundo" "(.*) mundo" "" ""
    gsubBS "fucking hard match" "hola munda, hola mundo." "ho([la]+) mu([ndoa]+)" "\\1 \\2" "la nda, la ndo."
    gsubBS "tricky match" "\"hello\"" "hello" "hola" "\"hola\""
    gsubBS "replacement"  "hola mundo" "(.*) mundo" "\\1" "hola"
    gsubBS "double replacement" "hola mundo como estas" "(.*) mundo" "\\1-\\1" "hola-hola como estas"
