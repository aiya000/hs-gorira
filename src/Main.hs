{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config
import Control.GoriraDB
import Control.GoriraMeCab
import Control.GoriraTwitter
import Control.Monad ( forM_ )
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )


-- Generate credential from oauth and token
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
      -- Generate new tweetMessage from fetched data and DB data,
      let tweets = map text timeline
      localMessages <- readDBTweets
      tweetMessage  <- generateTweet (localMessages ++ tweets) False
      -- Post tweetMessage to twitter
      postTweet oauth credential tweetMessage
      -- Cache read tweets
      putStrLn "\nThese tweet is cached: vvv"
      print tweets
      forM_ tweets $ \tweet -> addTweetToDB tweet
