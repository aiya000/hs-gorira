{-# LANGUAGE OverloadedStrings #-}
module Data.GoriraTwitter
  ( TimelineItem (..)
  , Timeline
  , TweetMessage
  , TwitterScreenName
  ) where
import Data.Aeson ( FromJSON (..), Value (..), (.:) )
import Data.Text ( Text () )
import Data.ByteString ( ByteString () )

-- The data type of "https://api.twitter.com/1.1/statuses/home_timeline.json" and other twitter jsons
data TimelineItem = TimelineItem
  { text :: Text
  } deriving ( Show )
instance FromJSON TimelineItem where
  parseJSON (Object v) = TimelineItem <$> v .: "text"

type Timeline          = [TimelineItem]
type TweetMessage      = ByteString
type TwitterScreenName = ByteString
