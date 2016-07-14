{-# LANGUAGE OverloadedStrings #-}

-- Module for Twitter API and Twitter Authentications

module Data.GoriraTwitter
  ( TimelineItem (..)
  , Timeline
  , TweetMessage
  , TwitterScreenName
  , TwitterAccessTokens (..)
  , TwitterAuth (..)
  ) where

import Data.Aeson ( FromJSON (..), Value (..), (.:) )
import Data.ByteString ( ByteString () )
import Data.Text ( Text () )
import Web.Authenticate.OAuth ( OAuth (), Credential () )

-- The data type of "https://api.twitter.com/1.1/statuses/home_timeline.json" and other twitter jsons
data TimelineItem = TimelineItem
  { text :: Text
  } deriving ( Show )
instance FromJSON TimelineItem where
  parseJSON (Object v) = TimelineItem <$> v .: "text"

type Timeline          = [TimelineItem]
type TweetMessage      = Text
type TwitterScreenName = ByteString


-- For Serialized file
data TwitterAccessTokens = TwitterAccessTokens
  { accessToken       :: ByteString
  , accessTokenSecret :: ByteString
  } deriving (Read)

-- data for Twitter Authentication
data TwitterAuth = TwitterAuth OAuth Credential
