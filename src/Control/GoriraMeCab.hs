{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraMeCab
  ( generateTweet
  ) where

import Control.SentenceJP ( generateSentence )
import Data.GoriraTwitter
import Data.Text ( replace )

generateTweet :: [TweetMessage] -> Bool -> IO TweetMessage
generateTweet tweets allowReplying =
  case allowReplying of
       True  -> generateSentence tweets
       False -> do
         sentence <- generateSentence tweets
         return . replace "@" "reply_to_" $ sentence
