{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import qualified Data.ByteString.Lazy as DBL
import           Control.Exception
import           Database.MongoDB
import           Database.MongoDB.Admin
import           Database.MongoDB.Query
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson (Value(..), object, (.=), encode, decode)
import qualified Data.Aeson.Text as DAT

import           Web.Scotty.Trans hiding (next, post, get, json)
import           Example (app, User (User), appConfigReader, DBInfo (DBInfo))
import           Data.UUID.V4
import qualified Data.UUID as DU
import           Data.Text

testUser = User "rob@example.com" "Rob" "+123456"
searchUser = User "fred@example.com" "Fred Frederiksen" "+12345"

dbNamespace = "rob_test_db"

getDBID :: IO String
getDBID = fmap ((++) dbNamespace) $ (fmap DU.toString $ nextRandom)

-- Shamelessly stolen from SO with minor modification
createTextIndex :: Collection -> String -> [Label] -> Action IO ()
createTextIndex col name keys = do
    db <- thisDatabase
    let doc = [ "ns"   =: intercalate "." [db, col]
              , "key"  =: [key =: ("text" :: String) | key <- keys]
              , "name" =: name
              ]
    insert_ "system.indexes" doc

createDB db = do
  pipe <- connect $ host "127.0.0.1"
  -- Unique index on the email column
  -- We could consider putting one on the phone number column also
  let idx = Index "user" ["email" =: (1 :: Integer)] "emailIdx" True True Nothing
  access pipe master db $ do
    createTextIndex "user" "name" ["name"]
    createIndex idx
    createIndex idx

dropDB db = do
  pipe <- connect $ host "127.0.0.1"
  putStrLn $ "Dropping database " ++ (unpack db)
  res <- access pipe master db $ do
    dropDatabase db
  putStrLn $ show res  

setUpApp db a = with (app'' db) $ a

app'' db = do
  scottyAppT (appConfigReader (DBInfo "127.0.0.1" db)) app

main :: IO ()
main = do
  db <- fmap pack getDBID
  createDB db
  putStrLn $ "Using database: " ++ (unpack db)
  
  hspec $ setUpApp db $ do

    describe "GET /customers" $ do
      it "responds with empty JSON" $ do
        get "/customers/doesnotexit" `shouldRespondWith` "[]"
        get "/customers/" `shouldRespondWith` "[]"

    describe "POST /customers (twice), expect failure" $ do
      it "should 409 when we create the same user twice" $ do
        (post "/customers/new" (encode $ testUser)
          `shouldRespondWith` 201)
        (get "/customers" `shouldRespondWith`
          [json|[{email: "rob@example.com", name: "Rob", phone: "+123456"}]|])
        (post "/customers/new" (encode $ testUser)
          `shouldRespondWith` 409)

    describe "Searching for existing customer should find said customer" $ do
      it "should return valid customer" $ do
        (post "/customers/new" (encode $ searchUser)
          `shouldRespondWith` "" {matchStatus = 201})
        (get "/customers/Fred" `shouldRespondWith` 
           [json|[{email: "fred@example.com", name: "Fred Frederiksen", phone: "+12345"}]|])

  dropDB db
