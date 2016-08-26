{-# LANGUAGE OverloadedStrings #-}

-- This module for Access to Twitter API

module Control.GoriraTwitter
  ( postTweet
  , fetchUserTimeline
  , fetchFollowersList
  ) where

import Data.Aeson (decode)
import Data.GoriraTwitter
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (newManager, parseRequest, setQueryString, urlEncodedBody, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (stdout)
import Web.Authenticate.OAuth (Credential, signOAuth, OAuth)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.GoriraTwitter.ApiTypes.Followers as Followers
import qualified Data.GoriraTwitter.ApiTypes.Statuses as Statuses

type Url       = String
type UrlParams = [(BSC.ByteString, Maybe BSC.ByteString)]


-- Fetch get or post request
httpsTwitterRequestWithParamsTo :: TwitterAuth -> Url -> UrlParams -> IO LBS.ByteString
httpsTwitterRequestWithParamsTo (TwitterAuth oauth credential) url params = do
  manager        <- newManager tlsManagerSettings
  requestModel   <- parseRequest url
  let requestWithParams = setQueryString params requestModel
  signedRequest  <- signOAuth oauth credential requestWithParams
  response       <- httpLbs signedRequest manager
  return $ responseBody response


-- Post a tweet
-- and return posted tweet message
--TODO: vvv
--postTweet :: TwitterAuth -> TweetMessage -> IO (Maybe Statuses.Update)
postTweet :: TwitterAuth -> TweetMessage -> IO TweetMessage
postTweet auth message = do
  let urlParams = [("status", Just $ encodeUtf8 message)]
  httpsTwitterRequestWithParamsTo auth "POST https://api.twitter.com/1.1/statuses/update.json" urlParams
  return message
  --fmap decode $ httpsTwitterRequestWithParamsTo auth "POST https://api.twitter.com/1.1/statuses/update.json" urlParams


-- Fetch {screenName}'s tweets as Timeline
-- See https://dev.twitter.com/rest/reference/get/statuses/user_timeline
fetchUserTimeline :: TwitterAuth -> TwitterScreenName -> IO (Maybe Statuses.UserTimeline)
fetchUserTimeline auth screenName = do
  let urlParams = [ ("include_rts", Just "false")
                  , ("count",       Just "80")
                  , ("screen_name", Just screenName)
                  ]
  fmap decode $ httpsTwitterRequestWithParamsTo auth "https://api.twitter.com/1.1/statuses/user_timeline.json" urlParams


-- Fetch {screenName}'s followers list
-- NOTE: Implement with cursor parameter if I needed
fetchFollowersList :: TwitterAuth -> TwitterScreenName -> IO (Maybe Followers.List)
fetchFollowersList auth screenName = do
  let urlParams = [("screen_name", Just screenName)]
  fmap decode $ httpsTwitterRequestWithParamsTo auth "https://api.twitter.com/1.1/followers/list.json" urlParams
