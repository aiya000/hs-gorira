{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraMeCab
import Control.GoriraTwitter
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )

newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

main :: IO ()
main = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  maybeTimeline <- fetchUserTimeline oauth credential "aiya_000"
  case maybeTimeline of
    Nothing       -> fail "failed fetching tweets"
    Just timeline -> do
      tweetMessage <- generateTweet timeline
      postTweet oauth credential tweetMessage
