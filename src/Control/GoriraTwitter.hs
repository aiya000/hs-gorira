{-# LANGUAGE OverloadedStrings #-}

-- This module for Access to Twitter API

module Control.GoriraTwitter
  ( postTweet
  , fetchUserTimeline
  ) where
import Data.Aeson ( decode )
import Data.Conduit ( ($$+-) )
import Data.GoriraTwitter
import Data.Text.Encoding ( encodeUtf8 )
import Network.HTTP.Conduit ( newManager, parseUrl, setQueryString, http, urlEncodedBody, responseBody, tlsManagerSettings, httpLbs )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary


-- TODO: return tweet status ( succeed or failed :: Bool )
-- Post a tweet
postTweet :: OAuth -> Credential -> TweetMessage -> IO ()
postTweet oauth credential message = do
  manager       <- newManager tlsManagerSettings
  request       <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let requestForPost = urlEncodedBody [("status", encodeUtf8 message)] request
  signedRequest <- signOAuth oauth credential requestForPost
  response      <- httpLbs signedRequest manager
  print $ responseBody response


-- Fetch screenName's tweets as Timeline
fetchUserTimeline :: OAuth -> Credential -> TwitterScreenName -> IO (Maybe Timeline)
fetchUserTimeline oauth credential screenName = do
  manager        <- newManager tlsManagerSettings
  requestModel   <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json"
  let requestForGet = setQueryString [ ("include_rts", Just "false")
                                     , ("count",       Just "80")
                                     , ("screen_name", Just screenName)
                                     ] requestModel
  signedRequest  <- signOAuth oauth credential requestForGet
  response       <- httpLbs signedRequest manager
  return . decode . responseBody $ response
