{-# LANGUAGE OverloadedStrings #-}

-- This module for generating tweet message

module Control.GoriraMeCab
  ( generateTweet
  ) where

import Control.GoriraTwitter
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.SentenceJP (generateSentence)
import Data.GoriraTwitter
import Data.MyException
import Data.Text (Text)
import qualified Data.GoriraTwitter.ApiTypes.Followers as Followers
import qualified Data.Text as T
import qualified Text.Regex.Posix as Regex

-- Temporary fake name in this source
type TwitterScreenName' = String


-- Generate sentence for twitter tweet
generateTweet :: (MonadThrow m, MonadIO m) => TwitterAuth -> [TweetMessage] -> Bool -> m TweetMessage
generateTweet auth tweets allowReply
  | allowReply = generateSentenceWithFilter auth tweets
  | otherwise  = generateSentenceWithoutReplying tweets
    where
      -- Generate sentence but ignore reply to not followers
      generateSentenceWithFilter :: (MonadThrow m, MonadIO m) => TwitterAuth -> [TweetMessage] -> m TweetMessage
      generateSentenceWithFilter auth tweets = do
        maybeScreenNames <- liftIO $ fmap (map Followers.listUsersScreenName . Followers.listUsers) <$> fetchFollowersList auth "aiya_gorira"
        sentence         <- generateSentence tweets
        case isReplyTweet $ T.unpack sentence of
          False -> return sentence
          True  -> case isReplyTo <$> maybeScreenNames <*> (Just sentence) of
                        Just True  -> return sentence
                        Just False -> return $ regulateReply sentence
                        Nothing    -> throwM $ IOException' "fetching followers list was failed"

      (=~#) :: String -> String -> Bool
      (=~#) = (Regex.=~)

      (=~$) :: String -> String -> String
      (=~$) = (Regex.=~)

      isReplyTweet :: String -> Bool
      isReplyTweet = (=~# "@[0-9A-Za-z_]+")

      isReplyTo :: [TwitterScreenName'] -> Text -> Bool
      isReplyTo xs y = let to = T.unpack y =~$ "@[0-9A-Za-z_]+"
                       in tail to `elem` xs  -- tail remove '@' prefix (ex: "@aiya_gorira" to "aiya_gorira")

      regulateReply :: Text -> Text
      regulateReply = T.replace "@" "reply_to_"

      -- Generate sentence but ignore reply message
      generateSentenceWithoutReplying :: MonadIO m => [TweetMessage] -> m TweetMessage
      generateSentenceWithoutReplying tweets = do
          sentence <- generateSentence tweets
          return $ regulateReply sentence
