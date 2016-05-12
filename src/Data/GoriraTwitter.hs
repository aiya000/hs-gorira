{-# LANGUAGE OverloadedStrings #-}
module Data.GoriraTwitter
  ( HomeTimelineItem (..)
  , HomeTimeline
  , TweetMessage
  ) where
import Data.Aeson ( FromJSON (..), Value (..), (.:) )
import Data.Text ( Text () )
import Data.ByteString ( ByteString () )

data HomeTimelineItem = HomeTimelineItem
  { text :: Text
  } deriving ( Show )
instance FromJSON HomeTimelineItem where
  parseJSON (Object v) = HomeTimelineItem <$> v .: "text"

type HomeTimeline = [HomeTimelineItem]

type TweetMessage = ByteString
