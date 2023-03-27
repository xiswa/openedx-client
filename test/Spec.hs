{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Data.Aeson

import Openedx.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON instances" $ do
    it "correctly encodes and decodes Actions" $ do
      encode Enroll         `shouldBe` "\"enroll\""
      encode Unenroll       `shouldBe` "\"unenroll\""
      decode "\"enroll\""   `shouldBe` Just Enroll
      decode "\"unenroll\"" `shouldBe` Just Unenroll
