{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Example (runApp, app, appConfigReader, User (User),
                DBInfo (DBInfo)) where

import           Control.Exception
import           GHC.Generics
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as DBL
import Network.HTTP.Types (notFound404, badRequest400, status409, status201)
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

data DBInfo = DBInfo {ip :: String, db :: Database}

data AppConfig = CustomerConfig {
    dbPool         :: (Pool Pipe)
  , runDBF         :: Action IO Cursor -> ActionD [Document]
  , dbInfo         :: DBInfo
}

type MongoDBError = String

data User = User { 
        email :: String, 
        name :: String,  
        phone :: String 
} deriving (Generic, Show) 
 
instance DA.ToJSON User where 
  toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON User

parseUser :: Document -> Maybe User
parseUser doc = do
  name <- cast' =<< look "name" doc
  email <- cast' =<< look "email" doc
  phone <- cast' =<< look "phone" doc
  Just $ User email name phone

parseUsers :: [Document] -> [Maybe User]
parseUsers docs = fmap parseUser docs

unparseUser :: User -> Document
unparseUser u = ["name" := val (name u),
                 "email" := val (email u),
                 "phone" := val (phone u)]

handleDBReadError :: Failure -> IO (Either MongoDBError Value)
handleDBReadError e = case e of
  -- When we put a unique index on a column we need to handle the case where
  -- the key already exists. This is inadequate error handling, but gets the
  -- job done for now.
  CompoundFailure cf -> return $ Left $ "Error user (probably) already exists"
  _                  -> return $ Left "Unhandled Error"

runDB :: Action IO Cursor -> ActionD [Document]
runDB a = do
  conf <- lift ask
  let pool = dbPool conf
  -- Here we just fetch all. When the collection contains many documents, we definitely
  -- want to paginate this.
  liftIO $ withResource pool (\pipe -> (access pipe master (database conf) (a >>= rest)))
    where
  database config = db $ dbInfo config

runDBInsert :: Action IO Value -> ActionD (Either MongoDBError Value)
runDBInsert a = do
  conf <- lift ask
  let pool = dbPool conf
  liftIO $ 
    catch (fmap Right $
            withResource pool (action (database conf) a)) handleDBReadError
    where
  database config = db $ dbInfo config
  action db a pipe = access pipe master db a

searchCustomersQuery searchTerm = (find $ select [ "$text" =: [ "$search" =: searchTerm ] ]  "user")

listCustomersQuery = (find $ select [] "user")

addCustomersQuery cust = (insert "user" $ unparseUser cust)

queryWithSerialisation :: Action IO Cursor -> ActionD ()
queryWithSerialisation query = do
  conf <- lift ask
  res <- (runDBF conf) query
  json $ parseUsers res

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
      e <- runDBInsert $ addCustomersQuery customer
      (case e of
        Left err          -> do
          status status409
          json err
        Right _           -> status status201)
    Nothing -> do
      status badRequest400
      json $ ("Could not parse user" :: String))

appConfigReader :: DBInfo -> ReaderT AppConfig IO a -> IO a
appConfigReader dbInfo r = do
  let ip = "127.0.0.1"
  dbPool <- createPool (connect $ host ip) close 1 300 5
  let appConfig = CustomerConfig dbPool runDB dbInfo
  runReaderT r appConfig

runApp :: IO ()
runApp = do
  let dbInfo = (DBInfo "127.0.0.1" "rob_db")
  scottyT 3000 (appConfigReader dbInfo) app

app :: ScottyD ()
app = do
  get  "/customers/:searchterm" searchCustomers
  get  "/customers" listCustomers
  post "/customers/new" addCustomer
