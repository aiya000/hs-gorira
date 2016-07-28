{-# LANGUAGE OverloadedStrings #-}

-- Module for behavior configs of hs-gorira

module Config
  ( readTwitterAuth
  , readGoriraConfig
  , IOException' (..)
  , ConfigTerm (..)
  , GoriraConfig
  ) where

import Control.Monad.Catch (MonadCatch, throwM, Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString hiding (readFile, writeFile)
import Data.GoriraTwitter
import Data.Map (Map, fromList)
import System.Directory (doesFileExist)
import Web.Authenticate.OAuth (OAuth, Credential, newCredential)

-- My IOException with String message
data IOException' = IOException' String
                  deriving (Show)
instance Exception IOException'

-- Config Value Wrapper
data ConfigTerm = TermBool Bool
                deriving (Show, Read)

-- hs-gorira behavior data type
type GoriraConfig = Map String ConfigTerm


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

-- Integrate reading Twitter Authentication data
readTwitterAuth :: IO TwitterAuth
readTwitterAuth = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  return $ TwitterAuth oauth credential


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
