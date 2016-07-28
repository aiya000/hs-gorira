
-- Module for Twitter API and Twitter Authentications

module Data.GoriraTwitter
  ( TweetMessage
  , TwitterScreenName
  , TwitterAccessTokens (..)
  , TwitterAuth (..)
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Web.Authenticate.OAuth (OAuth, Credential)


-- Type aliases
type TweetMessage      = Text
type TwitterScreenName = ByteString


-- For Serialized file
data TwitterAccessTokens = TwitterAccessTokens
  { accessToken       :: ByteString
  , accessTokenSecret :: ByteString
  } deriving (Read)

-- data for Twitter Authentication
data TwitterAuth = TwitterAuth OAuth Credential
