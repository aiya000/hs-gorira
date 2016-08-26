{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CmdOption
import Control.Config
import Control.GoriraDB
import Control.GoriraMeCab
import Control.GoriraTwitter
import Control.Monad (forM_, when)
import Control.Monad.Catch (SomeException, catch, try)
import Control.Monad.Trans.Either (runEitherT)
import Data.Config
import Data.GoriraTwitter
import Data.MyException
import Data.Set ((\\))
import Data.Text (pack, unpack)
import System.Console.CmdArgs (cmdArgs)
import qualified Data.GoriraTwitter.ApiTypes.Statuses as Statuses
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
goriraTweet :: TwitterAuth -> Statuses.UserTimeline -> Int -> IO ()
goriraTweet twitterAuth timeline count = do
  -- Generate new tweetMessage from fetched data and DB data,
  -- and Post tweetMessage to twitter
  prepareGoriraDB
  -- Read exists tweets
  let tweets  = map Statuses.userTimelineItemText timeline
  localMessages <- readDBTweets
  let tweets' = localMessages ++ tweets
  -- Read config
  config <- readGoriraConfig `catch` \(IOException' msg) -> fail msg
  case Map.lookup "allowReply" config of
    Nothing                    -> putStrLn "'allowReply'' was not found in config file"
    Just (TermBool allowReply) -> do
      tweetLoop twitterAuth tweets' count allowReply
      putStrLn "\nThese tweet to cache: vvv"
      cacheFetchedTweets tweets localMessages

-- X(
tweetLoop :: TwitterAuth -> [TweetMessage] -> Int -> Bool -> IO ()
tweetLoop twitterAuth tweets' count allowReply = do
  forM_ [1 .. count] $ \_ -> do
    eitherTweetMessage <- runEitherT $ generateTweet twitterAuth tweets' allowReply
    case eitherTweetMessage of
      Left (e :: SomeException) -> putStrLn $ "hs-gorira caught an error: " ++ show e
      Right tweetMessage        -> try (postTweet twitterAuth tweetMessage) >>= printPostResult

-- Cache "read tweets - exists records"
cacheFetchedTweets :: [TweetMessage] -> [TweetMessage] -> IO ()
cacheFetchedTweets tweets existsTweets = do
  let tweets' = Set.fromList tweets \\ Set.fromList existsTweets
  forM_ tweets' $ \tweet -> do
    TIO.putStrLn tweet
    addTweetToDB tweet

-- Print succeed or failed detail
printPostResult :: Either SomeException TweetMessage -> IO ()
printPostResult (Left e) = do
  putStrLn "\nhs-gorira cought tweeting error: vvv"
  print e
printPostResult (Right a) = do
  putStrLn "\nThis message was posted: vvv"
  TIO.putStrLn a
