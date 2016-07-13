{-# LANGUAGE OverloadedStrings #-}
module Main where

import CmdOption
import Config
import Control.GoriraDB
import Control.GoriraMeCab
import Control.GoriraTwitter
import Control.Monad ( forM_ )
import Data.GoriraTwitter
import System.Console.CmdArgs ( cmdArgs )
import Web.Authenticate.OAuth ( Credential (), newCredential )
import qualified Data.Text.IO as TIO


-- Generate credential from oauth and token
newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

-- Entry point
main :: IO ()
main = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  maybeTimeline <- fetchUserTimeline oauth credential "aiya_000"
  case maybeTimeline of
    Nothing       -> fail "failed fetching tweets"
    Just timeline -> do
      -- Get cmd argument values
      options <- cmdArgs tweetOptions
      let count = tweetCount options
      -- Generate new tweetMessage from fetched data and DB data,
      -- and Post tweetMessage to twitter
      let tweets  = map text timeline
      localMessages <- readDBTweets
      let tweets' = localMessages ++ tweets
      forM_ [1 .. count] $ \_ -> do
        tweetMessage <- generateTweet tweets' False
        postTweet oauth credential tweetMessage
      -- Cache read tweets
      putStrLn "\nThese tweet to cache: vvv"
      forM_ tweets $ \tweet -> do
        TIO.putStrLn tweet
        addTweetToDB tweet  --TODO: performance bottleneck -- O(n)
