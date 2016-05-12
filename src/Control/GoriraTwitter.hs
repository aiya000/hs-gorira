{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraTwitter
  ( tweet
  , fetchPublicTimeline
  ) where
import Data.Aeson ( decode )
import Data.Conduit ( ($$+-) )
import Data.GoriraTwitter
import Network.HTTP.Conduit ( newManager, parseUrl, http, urlEncodedBody, responseBody, tlsManagerSettings, httpLbs )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary


-- TODO: rename 'tweet' to 'postTweet'
-- TODO: return tweet status ( succeed or failed :: Bool )
tweet :: TweetMessage -> OAuth -> Credential -> IO ()
tweet message oauth credential = do
  manager       <- newManager tlsManagerSettings
  request       <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let requestForPost = urlEncodedBody [("status", message)] request
  signedRequest <- signOAuth oauth credential requestForPost
  response      <- httpLbs signedRequest manager
  print $ responseBody response


fetchPublicTimeline :: OAuth -> Credential -> IO (Maybe HomeTimeline)
fetchPublicTimeline oauth credential = do
  manager        <- newManager tlsManagerSettings
  requestForGet  <- parseUrl "https://api.twitter.com/1.1/statuses/home_timeline.json"
  signedRequest  <- signOAuth oauth credential requestForGet
  response       <- httpLbs signedRequest manager
  return . decode . responseBody $ response
