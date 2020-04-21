{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app, pool, appConfigReader, runQuery) where

import           Control.Exception
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Web.Scotty.Trans hiding (next)
import           Control.Monad.Reader
import           Data.Text.Lazy
import           Database.MongoDB
import           Database.MongoDB.Query
import           Data.Pool

type ScottyD = ScottyT Text (ReaderT AppConfig IO)
type ActionD = ActionT Text (ReaderT AppConfig IO)

data AppConfig = CustomerConfig {dbPool :: Pool Pipe}

ip = "127.0.0.1"
database = "krizo"

pool = createPool (connect $ host ip) close 1 300 5

runQuery :: Pool Pipe -> Action IO a -> IO a
runQuery pool query = withResource pool $
  (\pipe -> access pipe master database query)

appConfigReader :: ReaderT AppConfig IO a -> IO a
appConfigReader r = do
  dbPool <- pool
  let appConfig = CustomerConfig dbPool
  runReaderT r appConfig

runApp :: IO ()
runApp = do
  scottyT 3000 appConfigReader app'

app' :: ScottyD ()
app' = do
  get "/" $ do
    text "hello"

  get "/some-json" $ do
    json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = do
  scottyAppT appConfigReader app'
