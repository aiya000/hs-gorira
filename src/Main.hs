{-# LANGUAGE OverloadedStrings #-}

module Main where

import CmdOption
import Control.Config
import Control.GoriraDB
import Control.GoriraMeCab
import Control.Monad (forM_, when)
import Control.Monad.Catch (SomeException, catch, try)
import Control.Monad.Trans.Either (runEitherT)
import Data.Config
import Data.MyException
import Data.Set ((\\))
import Data.Text (pack, unpack)
import Data.TwiHigh
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import qualified Data.TwiHigh.Statuses as Statuses


-- Entry point
main :: IO ()
main = do
  eitherTwitterAuth <- runEitherT readTwitterAuth
  case eitherTwitterAuth of
    Left  e           -> fail e
    Right twitterAuth -> phaseOfFetchingUserTimeline twitterAuth

phaseOfFetchingUserTimeline :: TwitterAuth -> IO ()
phaseOfFetchingUserTimeline twitterAuth = do
  let urlParams = [ ("include_rts", Just "false")
                  , ("count",       Just "80")
                  , ("screen_name", Just "aiya_000")
                  ]
  maybeTimeline <- Statuses.fetchUserTimeline twitterAuth "aiya_000" urlParams
  case maybeTimeline of
    Nothing       -> fail "failed fetching tweets"
    Just timeline -> do
      -- Get cmd argument values
      options <- cmdArgs tweetOptions
      let count  = tweetCount options
      let tweets = map Statuses.userTimelineItemText timeline
      phaseOfCaching twitterAuth tweets count

phaseOfCaching :: TwitterAuth -> [TweetMessage] -> Int -> IO ()
phaseOfCaching twitterAuth tweets count = do
  -- Generate new tweetMessage from fetched data and DB data,
  -- and Post tweetMessage to twitter
  prepareGoriraDB
  -- Read exists tweets
  localMessages <- readDBTweets
  let tweets' = localMessages ++ tweets
  -- Append read tweets to DB
  putStrLn "\nThese tweet to cache: vvv"
  cacheFetchedTweets tweets localMessages
  -- Read config
  eitherConfig <- runEitherT readGoriraConfig
  case eitherConfig of
    Left  e      -> fail e
    Right config -> phaseOfTweeting twitterAuth tweets' count config

phaseOfTweeting :: TwitterAuth -> [TweetMessage] -> Int -> GoriraConfig -> IO ()
phaseOfTweeting twitterAuth tweets count config =
  case Map.lookup "allowReply" config of
    Nothing                    -> fail "'allowReply'' is not exists in the config file"
    Just (TermBool allowReply) -> forM_ [1 .. count] $ \_ -> tweeting twitterAuth tweets allowReply
    Just x                     -> fail $ "caught unknowned term: " ++ show x
  where
    tweeting twitterAuth tweets allowReply = do
      eitherTweetMessage <- runEitherT $ generateTweet twitterAuth tweets allowReply
      case eitherTweetMessage of
        Left  e            -> putStrLn $ "caught an error: " ++ show (e :: SomeException)
        Right tweetMessage -> try (Statuses.postTweet twitterAuth tweetMessage) >>= printPostResult

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
