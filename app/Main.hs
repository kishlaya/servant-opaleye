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

-- Postgresql Requires
import Opaleye
import Database.PostgreSQL.Simple
import Data.Profunctor.Product (p3)
import Control.Arrow hiding (app)
import Control.Monad.IO.Class


main :: IO ()
main = do
	conn <- connect ConnectInfo{connectHost="localhost", connectPort=5432, connectDatabase="mydb", connectPassword="b2b", connectUser="b2b"}
	allRows <- selectAllRows conn
	print allRows
	run 8081 (serve (Proxy :: Proxy UserAPI) (server conn))


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
server :: Connection -> Server UserAPI
server conn = fetchAll conn :<|> fetchUser conn :<|> create conn :<|> update conn :<|> delete conn 

-- Endpoint function handlers
fetchAll :: Connection -> Handler [User]
fetchAll conn = do
	rows <- liftIO (selectAllRows conn)
	liftIO (print rows)
	return (toUsers rows)

fetchUser :: Connection -> [Char] -> Handler [User]
fetchUser conn s = do
	rows <- liftIO (selectByName conn s)
	liftIO (print rows)
	return (toUsers rows)

create :: Connection -> User -> Handler User
create conn x@(User n e a) = do
	liftIO (insertRow conn (n, e, a))
	rows <- liftIO (selectAllRows conn)
	liftIO (print rows)
	return x

update :: Connection -> User -> Handler User
update conn x@(User n e a) = do
	liftIO (updateRow conn (n, e, a))
	rows <- liftIO (selectAllRows conn)
	liftIO (print rows)
	return x

delete :: Connection -> User -> Handler User
delete conn x@(User n e a) = do
	liftIO (deleteRow conn (n, e, a))
	rows <- liftIO (selectAllRows conn)
	liftIO (print rows)
	return x


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
