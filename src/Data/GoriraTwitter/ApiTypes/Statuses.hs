{-# LANGUAGE OverloadedStrings #-}

-- Module for https://dev.twitter.com/rest/reference/get/statuses/{some}

module Data.GoriraTwitter.ApiTypes.Statuses
  ( UserTimeline
  , UserTimelineItem (..)
  ) where

import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)


-- The data type of statuses/user_timeline
-- See https://dev.twitter.com/rest/reference/get/statuses/user_timeline
type UserTimeline = [UserTimelineItem]
data UserTimelineItem = UserTimelineItem
  { userTimelineItemText :: Text
  -- and more fields :P
  } deriving (Show)
instance FromJSON UserTimelineItem where
  parseJSON (Object v) = UserTimelineItem <$> v .: "text" -- and more items :P
  parseJSON _          = error "Data.GoriraTwitter.ApiType.UserTimelineItem: caught unexpected json field"
