{-# LANGUAGE OverloadedStrings #-}
module Main where

import CmdOption
import Config
import Control.GoriraDB
import Control.GoriraMeCab
import Control.GoriraTwitter
import Control.Monad (forM_, when)
import Control.Monad.Catch (SomeException, catch)
import Data.GoriraTwitter
import Data.GoriraTwitter.ApiTypes
import Data.Set ((\\))
import Data.Text (pack, unpack)
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  -- Read exists tweets
  let tweets  = map text timeline
  localMessages <- readDBTweets
  let tweets' = localMessages ++ tweets
  -- Read config
  config <- readGoriraConfig `catch` \(IOException' msg) -> fail msg
  case Map.lookup "allowReply" config of
    Nothing                    -> fail "'allowReply'' was not found in config file"
    Just (TermBool allowReply) -> do
      forM_ [1 .. count] $ \_ -> do
        tweetMessage <- generateTweet twitterAuth tweets' allowReply
        postTweet twitterAuth tweetMessage `catch` printTweetError
        putStrLn "\nThis message was posted: vvv"
        TIO.putStrLn tweetMessage
      putStrLn "\nThese tweet to cache: vvv"
      cacheFetchedTweets tweets localMessages

-- Cache "read tweets - exists records"
cacheFetchedTweets :: [TweetMessage] -> [TweetMessage] -> IO ()
cacheFetchedTweets tweets existsTweets = do
  let tweets' = Set.fromList tweets \\ Set.fromList existsTweets
  forM_ tweets' $ \tweet -> do
    TIO.putStrLn tweet
    addTweetToDB tweet

-- Print cought error to console
printTweetError :: SomeException -> IO ()
printTweetError e = do
  putStrLn "\nhs-gorira cought tweeting error: vvv"
  print e
