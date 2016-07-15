{-# LANGUAGE DeriveDataTypeable #-}

-- This module for command line options specifying

module CmdOption
  ( TweetOptions (..)
  , tweetOptions
  ) where

import System.Console.CmdArgs


data TweetOptions = TweetOptions
  { tweetCount :: Int
  } deriving (Data, Typeable)

tweetOptionSummary :: String
tweetOptionSummary = "hs-gorira post auto generated tweet\n" ++
                     "to @aiya_gorira account."

tweetOptions :: TweetOptions
tweetOptions = TweetOptions
  { tweetCount = 1 &= name "count" &= help "hs-gorira tweet nth" &= explicit
  }
  &= program "hs-gorira"
  &= summary tweetOptionSummary
