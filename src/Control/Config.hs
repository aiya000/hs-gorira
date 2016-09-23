{-# LANGUAGE OverloadedStrings #-}

-- Module for behavior configs of hs-gorira

module Control.Config
  ( readTwitterAuth
  , readGoriraConfig
  ) where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Config
import Data.GoriraTwitter
import Data.MyException
import System.Directory (doesFileExist)
import System.Posix.Env (getEnv)
import Web.Authenticate.OAuth (OAuth, Credential, newCredential)


-- Integrate reading Twitter Authentication data.
-- Read from ./resource/{twitter_oauth,twitter_access_tokens}
-- or value of $GORIRA_OAUTH and $GORIRA_ACCESS_TOKENS.
-- See ./resource/{twitter_oauth,twitter_access_tokens}.example
readTwitterAuth :: (MonadThrow m, MonadIO m) => m TwitterAuth
readTwitterAuth = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  return $ TwitterAuth oauth credential
    where
      -- Read Twitter OAuth
      -- from ./resource/twitter_oauth file or $HS_GORIRA_OAUTH value
      readOAuth :: (MonadThrow m, MonadIO m) => m OAuth
      readOAuth = do
        maybeOAuth <- liftIO $ getEnv "HS_GORIRA_OAUTH"
        case maybeOAuth of
          Just oauth -> return $ read oauth
          Nothing    -> ifM (liftIO $ doesFileExist "resource/twitter_oauth")
                          (liftIO $ read <$> readFile "resource/twitter_oauth")
                          (throwM $ IOException' "You must set $HS_GORIRA_OAUTH or create ./resource/twitter_oauth")

      -- Read Twitter AccessTokens
      -- from ./resource/twitter_access_tokens file or $HS_GORIRA_ACCESS_TOKENS value
      readAccessTokens :: (MonadThrow m, MonadIO m) => m TwitterAccessTokens
      readAccessTokens = do
        maybeAccessTokens <- liftIO $ getEnv "HS_GORIRA_ACCESS_TOKENS"
        case maybeAccessTokens of
          Just accessTokens -> return $ read accessTokens
          Nothing           -> ifM (liftIO $ doesFileExist "resource/twitter_access_tokens")
                                 (liftIO $ read <$> readFile "resource/twitter_access_tokens")
                                 (throwM $ IOException' "You must set $HS_GORIRA_ACCESS_TOKENS or create ./resource/twitter_access_tokens")

      -- Generate credential from twitter oauth and twitter token
      newCredential' :: TwitterAccessTokens -> Credential
      newCredential' accessTokens =
        let accessTokenValue       = accessToken accessTokens
            accessTokenSecretValue = accessTokenSecret accessTokens
        in newCredential accessTokenValue accessTokenSecretValue


-- Read general config of hs-gorira
-- from ./resource/hs-gorira-config file or $HS_GORIRA_CONFIG value
readGoriraConfig :: (MonadThrow m, MonadIO m) => m GoriraConfig
readGoriraConfig = do
  maybeConfig <- liftIO $ getEnv "HS_GORIRA_CONFIG"
  case maybeConfig of
    Just config -> return $ read config
    Nothing     -> ifM (liftIO $ doesFileExist "resource/hs-gorira-config")
                     (liftIO $ read <$> readFile "resource/hs-gorira-config")
                     (throwM $ IOException' "You must set $HS_GORIRA_CONFIG or create ./resource/hs-gorira-config")
