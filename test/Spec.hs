{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson (Value(..), object, (.=))

import           Example (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do

  describe "GET /customers" $ do
    it "responds with some JSON" $ do
      get "/customers" `shouldRespondWith` "\"[]\""

--jsonResponseMatcher status body = ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

--successfulJsonResp body = jsonResponseMatcher 200 body

--expectedJsonResponse = 
--  let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
--  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body
