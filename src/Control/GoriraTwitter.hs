{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraTwitter
  ( postTweet
  , fetchPublicTimeline
  ) where
import Data.Aeson ( decode )
import Data.Conduit ( ($$+-) )
import Data.GoriraTwitter
import Network.HTTP.Conduit ( newManager, parseUrl, http, urlEncodedBody, responseBody, tlsManagerSettings, httpLbs )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary


-- TODO: return tweet status ( succeed or failed :: Bool )
postTweet :: TweetMessage -> OAuth -> Credential -> IO ()
postTweet message oauth credential = do
  manager       <- newManager tlsManagerSettings
  request       <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let requestForPost = urlEncodedBody [("status", message)] request
  signedRequest <- signOAuth oauth credential requestForPost
  response      <- httpLbs signedRequest manager
  print $ responseBody response


fetchPublicTimeline :: OAuth -> Credential -> TwitterScreenName -> IO (Maybe Timeline)
fetchPublicTimeline oauth credential screenName = do
  manager        <- newManager tlsManagerSettings
  requestForGet  <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?include_rts=false&screen_name=" ++ screenName
  signedRequest  <- signOAuth oauth credential requestForGet
  response       <- httpLbs signedRequest manager
  return . decode . responseBody $ response
