{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Main where

-- Web API Requires
import Servant
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics
import Network.Wai.Handler.Warp
import Data.Char (toLower)
import Control.Monad.Reader hiding (ask)
import Control.Monad.Trans.Reader
import Data.Pool

-- Postgresql Requires
import Opaleye
import Database.PostgreSQL.Simple
import Data.Profunctor.Product (p3)
import Control.Arrow hiding (app)
import Control.Monad.IO.Class

-- Config
serverport = 8081
dbHostname = "localhost"
dbPort = 5432
dbName = "mydb"
dbUsername = "b2b"
dbPassword = "b2b"

main :: IO ()
main = getPool >>= flip withResource (\conn -> run serverport (app conn))
{-
main = do
    pool <- getPool
    withResource pool (\conn -> run 8081 (app conn))
-}

app :: Connection -> Application
app conn = serve (Proxy :: Proxy UserAPI) (enter (NT (naturalTransformer conn)) server)

getPool :: IO (Pool Connection)
getPool = createPool
    connectDb
    close
    1                         -- number of stripes
    (fromRational 10)         -- number of seconds a connection should remain in pool
    10                        -- max number of connections per stripe.
    where
        connectDb :: IO Connection
        connectDb = connect ConnectInfo {
                        connectHost=dbHostname,
                        connectPort=dbPort,
                        connectDatabase=dbName,
                        connectPassword=dbPassword,
                        connectUser=dbUsername
                    }


-- User type
data User = User { userName :: String, userEmail :: String, userAge :: Int } deriving (Eq, Show, Generic)

instance ToJSON User where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON User where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

toUsers :: [(String, String, Int)] -> [User]
toUsers = map (\(n, e, a) -> User n e a)


-- Endpoints specification
type UserAPI = "users" :> Get '[JSON] [User]
        :<|> "users" :> Capture "name" [Char] :> Get '[JSON] [User]
        :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
        :<|> "users" :> "update" :>  ReqBody '[JSON] User :> Post '[JSON] User
        :<|> "users" :> "delete" :>  ReqBody '[JSON] User :> Post '[JSON] User



-- Action to be taken at endpoints 
server :: ServerT UserAPI (ReaderT Connection IO)
server = fetchAll :<|> fetchUser :<|> create :<|> update :<|> delete


-- Endpoint function handlers
fetchAll :: ReaderT Connection IO [User]
fetchAll = do
    conn <- ask
    rows <- liftIO (selectAllRows conn)
    return (toUsers rows)

fetchUser :: [Char] -> ReaderT Connection IO [User]
fetchUser s = do
    conn <- ask
    rows <- liftIO (selectByName conn s)
    liftIO (print rows)
    return (toUsers rows)

create :: User -> ReaderT Connection IO User
create x@(User n e a) = do
    conn <- ask
    liftIO (insertRow conn (n, e, a))
    rows <- liftIO (selectAllRows conn)
    liftIO (print rows)
    return x

update :: User -> ReaderT Connection IO User
update x@(User n e a) = do
    conn <- ask
    liftIO (updateRow conn (n, e, a))
    rows <- liftIO (selectAllRows conn)
    liftIO (print rows)
    return x

delete :: User -> ReaderT Connection IO User
delete x@(User n e a) = do
    conn <- ask
    liftIO (deleteRow conn (n, e, a))
    rows <- liftIO (selectAllRows conn)
    liftIO (print rows)
    return x

naturalTransformer :: Connection -> ReaderT Connection IO a -> Handler a
naturalTransformer conn r = liftIO (runReaderT r conn)


-- DB Manipulation
userTable :: Table (Column PGText, Column PGText, Column PGInt4) (Column PGText, Column PGText, Column PGInt4)
userTable = Table "users" (p3 (required "uname", required "email", required "age"))

selectAllRows :: Connection -> IO [(String, String, Int)]
selectAllRows conn = runQuery conn $ queryTable userTable

selectByName :: Connection -> String -> IO [(String, String, Int)]
selectByName conn x = runQuery conn $ proc () -> do
    row@(u, _, _) <- queryTable userTable -< ()
    restrict -< (u .== constant x)
    returnA -< row

insertRow :: Connection -> (String, String, Int) -> IO ()
insertRow conn row = do
    runInsertMany conn userTable [(constant row)]
    return ()

updateRow :: Connection -> (String, String, Int) -> IO ()
updateRow conn row@(u, _, _) = do
    runUpdate conn userTable (\_ -> constant row) (\(x, _, _) -> x .== constant u)
    return ()

deleteRow :: Connection -> (String, String, Int) -> IO ()
deleteRow conn (u, _, _) = do
    runDelete conn userTable (\(x, _, _) -> x .== constant u)
    return ()
