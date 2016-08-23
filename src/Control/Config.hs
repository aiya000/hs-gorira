{-# LANGUAGE OverloadedStrings #-}

-- Module for behavior configs of hs-gorira

module Control.Config
  ( readTwitterAuth
  , readGoriraConfig
  ) where

import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Config
import Data.GoriraTwitter
import Data.MyException
import System.Directory (doesFileExist)
import Web.Authenticate.OAuth (OAuth, Credential, newCredential)


-- Integrate reading Twitter Authentication data
readTwitterAuth :: IO TwitterAuth
readTwitterAuth = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  return $ TwitterAuth oauth credential
    where
      -- Read Twitter OAuth from serialized data file
      readOAuth :: IO OAuth
      readOAuth = do
        oauth <- read <$> readFile "resource/twitter_oauth"
        return oauth

      -- Read Twitter AccessTokens from serialized data file
      readAccessTokens :: IO TwitterAccessTokens
      readAccessTokens = do
        accessTokens <- read <$> readFile "resource/twitter_access_tokens"
        return accessTokens

      -- Generate credential from twitter oauth and twitter token
      newCredential' :: TwitterAccessTokens -> Credential
      newCredential' accessTokens =
        let accessTokenValue       = accessToken accessTokens
            accessTokenSecretValue = accessTokenSecret accessTokens
        in newCredential accessTokenValue accessTokenSecretValue


goriraConfigFilePath :: FilePath
goriraConfigFilePath = "resource/hs-gorira-config"

-- Read general config of hs-gorira
readGoriraConfig :: (MonadCatch m, MonadIO m) => m GoriraConfig
readGoriraConfig = do
  b <- liftIO $ doesFileExist goriraConfigFilePath
  if b then read <$> (liftIO $ readFile goriraConfigFilePath)
       else throwM $ IOException' "config file is not found"

---- Save general config of hs-gorira
--writeGoriraConfig :: GoriraConfig -> IO ()
--writeGoriraConfig config = writeFile goriraConfigFilePath $ show config
