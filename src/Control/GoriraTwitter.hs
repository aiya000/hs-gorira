{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraTwitter
  ( tweet
  ) where
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource.Internal ( MonadResource () )
import Data.ByteString ( ByteString () )
import Data.Conduit ( ($$+-) )
import Network.Connection ( TLSSettings (TLSSettingsSimple) )
import Network.HTTP.Conduit ( newManager, parseUrl, http, urlEncodedBody, mkManagerSettings, responseBody )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary

-- TODO: return tweet status ( succeed or failed )
tweet :: MonadResource m => ByteString -> OAuth -> Credential -> m ()
tweet message oauth credential = do
  let settings    = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager       <- liftIO $ newManager settings
  request       <- liftIO $ parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let postRequest = urlEncodedBody [("status", message)] request
  signedRequest <- signOAuth oauth credential postRequest
  response      <- http signedRequest manager
  responseBody response $$+- CBinary.sinkHandle stdout
