{-# LANGUAGE OverloadedStrings #-}

-- Module for https://dev.twitter.com/rest/reference/get/followers/{some}

module Data.GoriraTwitter.ApiTypes.Followers
  ( List (..)
  , ListUsers (..)
  ) where

import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)


---- See https://dev.twitter.com/rest/reference/get/followers/ids
--data Ids = Ids
--  { idsIds               :: [Integer]
--  , idsNextCursor        :: Integer
--  , idsNextCursorStr     :: String
--  , idsPreviousCursor    :: Integer
--  , idsPreviousCursorStr :: String
--  } deriving (Show)
--instance FromJSON Ids where
--  parseJSON (Object v) =
--    Ids <$> v .: "ids"
--        <*> v .: "next_cursor"
--        <*> v .: "next_cursor_str"
--        <*> v .: "previous_cursor"
--        <*> v .: "previous_cursor_str"
--  parseJSON _ = error "Data.GoriraTwitter.ApiType.Followers.Ids: caught unexpected json field"


-- See https://dev.twitter.com/rest/reference/get/followers/list
data List = List
  { listUsers             :: [ListUsers]
  , listNextCursor        :: Integer
  , listNextCursorStr     :: String
  , listPreviousCursor    :: Integer
  , listPreviousCursorStr :: String
  } deriving (Show)
instance FromJSON List where
  parseJSON (Object v) =
    List <$> v .: "users"
         <*> v .: "next_cursor"
         <*> v .: "next_cursor_str"
         <*> v .: "previous_cursor"
         <*> v .: "previous_cursor_str"
  parseJSON _ = error "Data.GoriraTwitter.ApiType.Followers.List: caught unexpected json field"

-- An item for List
data ListUsers = ListUsers
  { listUsersId              :: Integer
  , listUsersIdStr           :: String
  , listUsersName            :: Text
  , listUsersScreenName      :: String
  --, listUsersLocation        :: ???
  --, listUsersUrl             :: ???
  , listUsersDescription     :: String
  , listUsersProtected       :: Bool
  , listUsersFollowersCount  :: Integer
  , listUsersFriendsCount    :: Integer
  , listUsersListedCount     :: Integer
  , listUsersCreatedAt       :: String
  , listUsersFavouritesCount :: Integer
  -- and more fields :P
  } deriving (Show)
instance FromJSON ListUsers where
  parseJSON (Object v) =
    ListUsers <$> v .: "id"
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
  parseJSON _ = error "Data.GoriraTwitter.ApiType.Followers.ListUsers: caught unexpected json field"
