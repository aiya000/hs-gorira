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
import Data.Set ((\\))
import Data.Text (pack, unpack)
import System.Console.CmdArgs (cmdArgs)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import qualified Text.Regex.Posix as RPosix


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
  cacheFetchedTweets tweets localMessages

-- Cache "read tweets - exists records"
cacheFetchedTweets :: [TweetMessage] -> [TweetMessage] -> IO ()
cacheFetchedTweets tweets existsTweets = do
  let tweets'  = Set.fromList tweets \\ Set.fromList existsTweets
  let tweets'' = ignoreReplies tweets'
  forM_ tweets'' $ \tweet -> do
    TIO.putStrLn tweet
    addTweetToDB tweet
  where
    ignoreReplies :: Set.Set TweetMessage -> Set.Set TweetMessage
    ignoreReplies = Set.map pack . Set.filter (\s -> not $ s =~ "@\\w+") . Set.map unpack
    (=~) :: String -> String -> Bool
    (=~) = (RPosix.=~)

-- Print cought error to console
printTweetError :: SomeException -> IO ()
printTweetError e = do
  putStrLn "\nhs-gorira cought tweeting error: vvv"
  print e
