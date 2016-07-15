{-# LANGUAGE OverloadedStrings #-}

-- This module for Access to Twitter API

module Control.GoriraTwitter
  ( postTweet
  , fetchUserTimeline
  ) where

import Data.Aeson ( decode )
import Data.ByteString.Lazy.Char8 ( unpack )
import Data.Conduit ( ($$+-) )
import Data.GoriraTwitter
import Data.Text.Encoding ( encodeUtf8 )
import Network.HTTP.Conduit ( newManager, parseUrl, setQueryString, http, urlEncodedBody, responseBody, tlsManagerSettings, httpLbs )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary


-- Post a tweet
-- and return posted tweet message if succeed
postTweet :: TwitterAuth -> TweetMessage -> IO ()
postTweet (TwitterAuth oauth credential) message = do
  manager       <- newManager tlsManagerSettings
  request       <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let requestForPost = urlEncodedBody [("status", encodeUtf8 message)] request
  signedRequest <- signOAuth oauth credential requestForPost
  httpLbs signedRequest manager
  return ()


-- Fetch screenName's tweets as Timeline
fetchUserTimeline :: TwitterAuth -> TwitterScreenName -> IO (Maybe Timeline)
fetchUserTimeline (TwitterAuth oauth credential) screenName = do
  manager        <- newManager tlsManagerSettings
  requestModel   <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json"
  let requestForGet = setQueryString [ ("include_rts", Just "false")
                                     , ("count",       Just "80")
                                     , ("screen_name", Just screenName)
                                     ] requestModel
  signedRequest  <- signOAuth oauth credential requestForGet
  response       <- httpLbs signedRequest manager
  return . decode . responseBody $ response
