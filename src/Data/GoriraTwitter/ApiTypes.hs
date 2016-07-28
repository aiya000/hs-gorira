{-# LANGUAGE OverloadedStrings #-}

-- Module for https://dev.twitter.com/rest/reference 's types

module Data.GoriraTwitter.ApiTypes
  ( TimelineItem (..)
  , Timeline
  --, FollowerIds (..)
  , FollowersList (..)
  , FollowersListUsers (..)
  ) where

import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)


-- Type aliases

type Timeline = [TimelineItem]

--


-- The data type of statuses/user_timeline
-- See https://api.twitter.com/1.1/statuses/user_timeline.json and other twitter jsons
data TimelineItem = TimelineItem
  { text :: Text
  } deriving (Show)
instance FromJSON TimelineItem where
  parseJSON (Object v) = TimelineItem <$> v .: "text"
  parseJSON _          = error "Data.GoriraTwitter.ApiType.TimelineItem: caught unexpected json field"


---- See https://dev.twitter.com/rest/reference/get/followers/ids
--data FollowersIds = FollowerIds
--  { followersIdIds               :: [Integer]
--  , followersIdNextCursor        :: Integer
--  , followersIdNextCursorStr     :: String
--  , followersIdPreviousCursor    :: Integer
--  , followersIdPreviousCursorStr :: String
--  } deriving (Show)
--instance FromJSON FollowersIds where
--  parseJSON (Object v) =
--    FollowersIds <$> v .: "ids"
--                 <*> v .: "next_cursor"
--                 <*> v .: "next_cursor_str"
--                 <*> v .: "previous_cursor"
--                 <*> v .: "previous_cursor_str"
--  parseJSON _ = error "Data.GoriraTwitter.ApiType.FollowersIds: caught unexpected json field"


-- See https://dev.twitter.com/rest/reference/get/followers/list
data FollowersList = FollowersList
  { followersListUsers             :: [FollowersListUsers]
  , followersListNextCursor        :: Integer
  , followersListNextCursorStr     :: String
  , followersListPreviousCursor    :: Integer
  , followersListPreviousCursorStr :: String
  } deriving (Show)
instance FromJSON FollowersList where
  parseJSON (Object v) =
    FollowersList <$> v .: "users"
                  <*> v .: "next_cursor"
                  <*> v .: "next_cursor_str"
                  <*> v .: "previous_cursor"
                  <*> v .: "previous_cursor_str"
  parseJSON _ = error "Data.GoriraTwitter.ApiType.FollowersList: caught unexpected json field"

-- An item for FollowersList
data FollowersListUsers = FollowersListUsers
  { followersListUsersId              :: Integer
  , followersListUsersIdStr           :: String
  , followersListUsersName            :: Text
  , followersListUsersScreenName      :: String
  --, followersListUsersLocation        :: ???
  --, followersListUsersUrl             :: ???
  , followersListUsersDescription     :: String
  , followersListUsersProtected       :: Bool
  , followersListUsersFollowersCount  :: Integer
  , followersListUsersFriendsCount    :: Integer
  , followersListUsersListedCount     :: Integer
  , followersListUsersCreatedAt       :: String
  , followersListUsersFavouritesCount :: Integer
  -- and more fields :P
  } deriving (Show)
instance FromJSON FollowersListUsers where
  parseJSON (Object v) =
    FollowersListUsers <$> v .: "id"
                       <*> v .: "id_str"
                       <*> v .: "name"
                       <*> v .: "screen_name"
                       -- <*> v .: "location"
                       -- <*> v .: "url"
                       <*> v .: "description"
                       <*> v .: "protected"
                       <*> v .: "followers_count"
                       <*> v .: "friends_count"
                       <*> v .: "listed_count"
                       <*> v .: "created_at"
                       <*> v .: "favourites_count"
                       -- and more items :P
  parseJSON _ = error "Data.GoriraTwitter.ApiType.FollowersListUsers: caught unexpected json field"
