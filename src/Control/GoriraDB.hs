{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- This module for accessing application's database

module Control.GoriraDB
  ( prepareGoriraDB
  , addTweetToDB
  , readDBTweets
  ) where

import Control.Monad (void)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text (Text)
import Data.TwiHigh
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- | A monad to write data to sqlite
type Sqlite a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a


-- Define data type and function dynamically
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TweetCache
    tweetMessage TweetMessage
    deriving Show
|]

-- The App's DB file
dbFile :: Text
dbFile = "tweets.sqlite3"


-- You must call this function before using this module functions
prepareGoriraDB :: IO ()
prepareGoriraDB = runSqlite dbFile $ runMigration migrateAll


-- Add TweetMessage to local DB
addTweetToDB :: TweetMessage -> IO ()
addTweetToDB = runSqlite dbFile . void' . insert . TweetCache
  where
    void' :: Sqlite a -> Sqlite ()
    void' = void


-- Read [TweetMessage] from local DB
readDBTweets :: IO [TweetMessage]
readDBTweets = do
  records <- runSqlite dbFile selectAllRecord
  -- tweetCacheTweetMessage is tweetMessage
  return $ map (tweetCacheTweetMessage . entityVal) records
    where
      selectAllRecord :: Sqlite [Entity TweetCache]
      selectAllRecord = selectList [] []
