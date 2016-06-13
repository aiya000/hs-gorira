module Control.GoriraMeCab
  ( generateTweet
  ) where

import Data.GoriraTwitter
import Control.SentenceJP ( generateSentence )

generateTweet :: Timeline -> IO TweetMessage
generateTweet timeline = do
  let tweets = map text timeline
  generateSentence tweets
