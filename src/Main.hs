{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource ( runResourceT )
import Data.Conduit ( ($$+-) )
import Network.Connection ( TLSSettings (TLSSettingsSimple) )
import Network.HTTP.Conduit ( newManager, parseUrl, http, urlEncodedBody, mkManagerSettings, responseBody )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), newCredential, signOAuth )
import qualified Data.Conduit.Binary as CBinary


newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

main :: IO ()
main = do
  oauth        <- readOAuth
  accessTokens <- readAccessTokens
  let credential = newCredential' accessTokens
  runResourceT $ do
    let settings    = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager       <- liftIO $ newManager settings
    request       <- liftIO $ parseUrl "http://twitter.com/statuses/update.json"
    let postRequest = urlEncodedBody [("status", "Hello world!")] request
    signedRequest <- signOAuth oauth credential postRequest
    response      <- http signedRequest manager
    responseBody response $$+- CBinary.sinkHandle stdout
