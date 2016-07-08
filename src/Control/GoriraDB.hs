{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Control.GoriraDB
  ( addTweetToDB
  , readDBTweets
  ) where

import Data.GoriraTwitter
import Data.Text ( Text )
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TweetCache
    tweetMessage TweetMessage
    deriving Show
|]

dbFile :: Text
dbFile = "tweets.sqlite3"


-- Add TweetMessage to local DB
addTweetToDB :: TweetMessage -> IO ()
addTweetToDB tweet = runSqlite dbFile $ do
  runMigration migrateAll
  insert $ TweetCache tweet
  return ()

-- Read all record from dbFile's TweetCache table
selectAllRecord = do
  runMigration migrateAll
  xs  <- selectList [] []
  return (xs :: [Entity TweetCache])

-- Read [TweetMessage] from local DB
readDBTweets :: IO [TweetMessage]
readDBTweets = do
  records <- runSqlite dbFile $ selectAllRecord
  -- tweetCacheTweetMessage is tweetMessage
  let records' = map (tweetCacheTweetMessage . entityVal) records
  return records'
