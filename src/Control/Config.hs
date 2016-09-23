{-# LANGUAGE OverloadedStrings #-}

-- Module for behavior configs of hs-gorira

module Control.Config
  ( readTwitterAuth
  , readGoriraConfig
  ) where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Config
import Data.GoriraTwitter
import Data.MyException
import System.Directory (doesFileExist)
import Web.Authenticate.OAuth (OAuth, Credential, newCredential)


-- Integrate reading Twitter Authentication data
readTwitterAuth :: (MonadThrow m, MonadIO m) => m TwitterAuth
readTwitterAuth = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  return $ TwitterAuth oauth credential
    where
      -- Read Twitter OAuth from serialized data file
      readOAuth :: (MonadThrow m, MonadIO m) => m OAuth
      readOAuth = do
        x <- liftIO $ doesFileExist "resource/twitter_oauth"
        if x then liftIO $ read <$> readFile "resource/twitter_oauth"
             else throwM $ IOException' "'resource/twitter_oauth' is not exists"

      -- Read Twitter AccessTokens from serialized data file
      readAccessTokens :: (MonadThrow m, MonadIO m) => m TwitterAccessTokens
      readAccessTokens = do
        x <- liftIO $ doesFileExist "resource/twitter_access_tokens"
        if x then liftIO $ read <$> readFile "resource/twitter_access_tokens"
             else throwM $ IOException' "'resource/twitter_access_tokens' is not exists"

      -- Generate credential from twitter oauth and twitter token
      newCredential' :: TwitterAccessTokens -> Credential
      newCredential' accessTokens =
        let accessTokenValue       = accessToken accessTokens
            accessTokenSecretValue = accessTokenSecret accessTokens
        in newCredential accessTokenValue accessTokenSecretValue


-- Read general config of hs-gorira
readGoriraConfig :: (MonadThrow m, MonadIO m) => m GoriraConfig
readGoriraConfig = do
  x <- liftIO $ doesFileExist "resource/hs-gorira-config"
  if x then liftIO $ read <$> readFile "resource/hs-gorira-config"
       else throwM $ IOException' "'resource/hs-gorira-config' is not exists"

---- Save general config of hs-gorira
--writeGoriraConfig :: GoriraConfig -> IO ()
--writeGoriraConfig config = writeFile "resource/hs-gorira-config" $ show config
