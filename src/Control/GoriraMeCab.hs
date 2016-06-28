{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraMeCab
  ( generateTweet
  ) where

import Control.SentenceJP ( generateSentence )
import Data.GoriraTwitter
import Data.Text ( replace )

generateTweet :: Timeline -> Bool -> IO TweetMessage
generateTweet timeline allowReplying = do
  let tweets = map text timeline
  case allowReplying of
       True  -> generateSentence tweets
       False -> do
         sentence <- generateSentence tweets
         return . replace "@" "reply_to_" $ sentence
