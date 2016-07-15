{-# LANGUAGE OverloadedStrings #-}

-- Module for Twitter OAuth

module Config
  ( readTwitterAuth
  ) where

import Data.ByteString hiding (readFile)
import Data.GoriraTwitter
import Web.Authenticate.OAuth (OAuth, Credential, newCredential)


-- Read OAuth from serialized data file
readOAuth :: IO OAuth
readOAuth = do
  oauth <- read <$> readFile "resource/twitter_oauth"
  return oauth

-- Read Twitter AccessTokens from serialized data file
readAccessTokens :: IO TwitterAccessTokens
readAccessTokens = do
  accessTokens <- read <$> readFile "resource/twitter_access_tokens"
  return accessTokens

-- Generate credential from oauth and token
newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

-- Integrate reading Authentication data
readTwitterAuth :: IO TwitterAuth
readTwitterAuth = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  return $ TwitterAuth oauth credential
