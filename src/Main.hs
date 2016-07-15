{-# LANGUAGE OverloadedStrings #-}
module Main where

import CmdOption
import Config
import Control.Exception (SomeException, catch)
import Control.GoriraDB
import Control.GoriraMeCab
import Control.GoriraTwitter
import Control.Monad (forM_)
import Data.GoriraTwitter
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Maybe as DMaybe
import qualified Data.Text.IO as TIO


-- Entry point
main :: IO ()
main = do
  twitterAuth   <- readTwitterAuth
  maybeTimeline <- fetchUserTimeline twitterAuth "aiya_000"
  case maybeTimeline of
    Nothing       -> fail "failed fetching tweets"
    Just timeline -> do
      -- Get cmd argument values
      options <- cmdArgs tweetOptions
      let count = tweetCount options
      goriraTweet twitterAuth timeline count

-- Body of tweet logic
goriraTweet :: TwitterAuth -> Timeline -> Int -> IO ()
goriraTweet twitterAuth timeline count = do
  -- Generate new tweetMessage from fetched data and DB data,
  -- and Post tweetMessage to twitter
  prepareGoriraDB
  let tweets  = map text timeline
  localMessages <- readDBTweets
  let tweets' = localMessages ++ tweets
  forM_ [1 .. count] $ \_ -> do
    tweetMessage <- generateTweet tweets' False
    postTweet twitterAuth tweetMessage `catch` printTweetError
    putStrLn "\nThis message was posted: vvv"
    TIO.putStrLn tweetMessage
  putStrLn "\nThese tweet to cache: vvv"
  cacheFetchedTweets tweets

-- Cache read tweets
cacheFetchedTweets :: [TweetMessage] -> IO ()
cacheFetchedTweets tweets = do
  forM_ tweets $ \tweet -> do
    TIO.putStrLn tweet
    addTweetToDB tweet

-- Print cought error to console
printTweetError :: SomeException -> IO ()
printTweetError e = do
  putStrLn "\nhs-gorira cought tweeting error: vvv"
  print e
