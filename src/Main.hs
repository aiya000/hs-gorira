{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource ( runResourceT )
import Data.Conduit ( ($$+-) )
import Network.Connection ( TLSSettings (TLSSettingsSimple) )
import Network.HTTP.Conduit ( newManager, parseUrl, http, urlEncodedBody, mkManagerSettings, responseBody )
import System.IO ( stdout )
import Web.Authenticate.OAuth ( Credential (), newCredential, signOAuth, OAuth )
import qualified Data.Conduit.Binary as CBinary
import Control.Monad.Trans.Resource.Internal (MonadResource ())


newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

tweet :: MonadResource m => OAuth -> Credential -> m ()
tweet oauth credential = do
  let settings    = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager       <- liftIO $ newManager settings
  request       <- liftIO $ parseUrl "https://api.twitter.com/1.1/statuses/update.json"
  let postRequest = urlEncodedBody [("status", "Hello world!")] request
  signedRequest <- signOAuth oauth credential postRequest
  response      <- http signedRequest manager
  liftIO $ putStrLn "( ><) posted"
  responseBody response $$+- CBinary.sinkHandle stdout

main :: IO ()
main = do
  oauth        <- readOAuth
  accessTokens <- readAccessTokens
  let credential = newCredential' accessTokens
  runResourceT $ tweet oauth credential
