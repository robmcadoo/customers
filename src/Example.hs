{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Example (runApp, app, appConfigReader) where


import           Control.Exception
import           GHC.Generics
import qualified Data.Aeson as DA
import qualified Data.Aeson.Text as DAT
import qualified Data.ByteString.Lazy as DBL
import Network.HTTP.Types (notFound404, badRequest400)
import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Web.Scotty.Trans hiding (next)
import           Control.Monad.Reader
import           Data.Text.Lazy hiding (find)
import           Database.MongoDB
import           Database.MongoDB.Query
import           Data.Pool

type ScottyD = ScottyT Text (ReaderT AppConfig IO)
type ActionD = ActionT Text (ReaderT AppConfig IO)

data AppConfig = CustomerConfig {
    dbPool :: (Pool Pipe)
  , runDBF :: Action IO Cursor -> ActionD [Document]
}

data User = User { 
        email :: String, 
        name :: String,  
        phone :: String 
} deriving (Generic, Show) 
 
instance DA.ToJSON User where 
  toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON User

ip = "127.0.0.1"
database = "krizo"

parseUser :: Document -> Maybe User
parseUser doc = do
  name <- cast' =<< look "name" doc
  email <- cast' =<< look "email" doc
  phone <- cast' =<< look "phone" doc
  Just $ User name email phone

parseUsers :: [Document] -> [Maybe User]
parseUsers docs = fmap parseUser docs

unparseUser :: User -> Document
unparseUser u = ["name" := val (name u),
                 "email" := val (email u),
                 "phone" := val (phone u)]

runDB :: Action IO Cursor -> ActionD [Document]
runDB a = do
  conf <- lift ask
  let pool = dbPool conf
  -- Here we just fetch all. When the collection contains many documents, we definitely
  -- want to paginate this.
  liftIO $ withResource pool (\pipe -> (access pipe master "krizo" (a >>= rest)))

runDBInsert :: Action IO Value -> ActionD Value
runDBInsert a = do
  conf <- lift ask
  let pool = dbPool conf
  liftIO $ withResource pool (\pipe -> (access pipe master "krizo" a))

searchCustomersQuery searchTerm = (find $ select [ "$text" =: [ "$search" =: searchTerm ] ]  "user")

listCustomersQuery = (find $ select [] "user")

addCustomersQuery cust = (insert "user" $ unparseUser cust)

queryWithSerialisation :: Action IO Cursor -> ActionD ()
queryWithSerialisation query = do
  conf <- lift ask
  res <- (runDBF conf) listCustomersQuery
  json $ DAT.encodeToLazyText $ parseUsers res

listCustomers :: ActionD ()
listCustomers = queryWithSerialisation listCustomersQuery

searchCustomers :: ActionD ()
searchCustomers = do 
  searchTerm <- param "searchterm"
  queryWithSerialisation (searchCustomersQuery (searchTerm :: String))

-- We could refactor reusable logic out of here, but for now there seems to be little
-- value in that since we have only one endpoint that does inserts. TODO for the future
addCustomer :: ActionD ()
addCustomer = do
  reqBody <- body
  let customerM = (DA.decode $ (reqBody :: DBL.ByteString)) :: Maybe User
  (case customerM of
    Just customer -> do 
      runDBInsert $ addCustomersQuery customer
      json $ DAT.encodeToLazyText ([] :: [String])
    Nothing -> do
      status badRequest400
      json $ DAT.encodeToLazyText ("Could not parse user" :: String))

appConfigReader :: ReaderT AppConfig IO a -> IO a
appConfigReader r = do
  dbPool <- createPool (connect $ host ip) close 1 300 5
  let appConfig = CustomerConfig dbPool runDB
  runReaderT r appConfig

runApp :: IO ()
runApp = do
  scottyT 3000 appConfigReader app

app :: ScottyD ()
app = do
  get  "/customers" listCustomers
  get  "/customers/:searchterm" searchCustomers
  post "/customers/new" addCustomer

--app :: IO Application
--app = do
--  scottyAppT appConfigReader app'
