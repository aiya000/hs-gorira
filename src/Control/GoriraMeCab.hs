{-# LANGUAGE OverloadedStrings #-}

-- This module for generating tweet message

module Control.GoriraMeCab
  ( generateTweet
  ) where

import Control.GoriraTwitter
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.SentenceJP (generateSentence)
import Data.GoriraTwitter
import Data.Maybe (fromJust)
import Data.MyException
import Data.Text (Text)
import Text.Regex.Applicative.Text (RE', (<|>))
import qualified Data.GoriraTwitter.ApiTypes.Followers as Followers
import qualified Data.Text as T
import qualified Text.Regex.Applicative.Text as RT
import qualified Text.Regex.Posix as Regex

-- Temporary fake name in this source
type TwitterScreenName' = String


-- Generate sentence for twitter tweet
generateTweet :: (MonadCatch m, MonadIO m) => TwitterAuth -> [TweetMessage] -> Bool -> m TweetMessage
generateTweet auth tweets allowReply
  | allowReply = generateSentenceWithFilter auth tweets
  | otherwise  = generateSentenceWithoutReplying tweets
    where
      -- Generate sentence but ignore reply to not followers
      generateSentenceWithFilter :: (MonadCatch m, MonadIO m) => TwitterAuth -> [TweetMessage] -> m TweetMessage
      generateSentenceWithFilter auth tweets = do
        maybeScreenNames <- liftIO $ fmap (map Followers.listUsersScreenName . Followers.listUsers) <$> fetchFollowersList auth "aiya_gorira"
        sentence         <- generateSentence tweets
        case isReplyTweet sentence of
          False -> return sentence
          True  -> case isReplyTo <$> maybeScreenNames <*> (Just sentence) of
                        Just True  -> return $ beExplicitReply $ sentence
                        Just False -> return $ regulateReply . beExplicitReply $ sentence
                        Nothing    -> throwM $ IOException' "fetching followers list was failed"

      (=~#) :: Text -> RE' [Char] -> Bool
      txt =~# regex = case txt RT.=~ regex of
                            Nothing -> False
                            Just  _ -> True

      (=~$) :: Text -> RE' [Char] -> Maybe [Char]
      (=~$) = (RT.=~)

      -- For screen_name's regex
      alphanum' :: Char -> Bool
      alphanum' c = ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || (c == '_')

      -- This means "@[0-9a-zA-Z_]+" for Regex.Text.Applicative.Text (=~)
      screenNameSym :: RE' [Char]
      screenNameSym = RT.many $ RT.sym '@' <|> RT.psym alphanum'

      -- The regex for the message is reply
      -- This means "@[0-9a-zA-Z_]+.*" for Regex.Text.Applicative.Text (=~)
      startWithScreenNameSym :: RE' [Char]
      startWithScreenNameSym = RT.many $ RT.sym '@' <|> RT.psym alphanum' <|> RT.anySym

      isReplyTweet :: Text -> Bool
      isReplyTweet = (=~# startWithScreenNameSym)

      isReplyTo :: [TwitterScreenName'] -> Text -> Bool
      isReplyTo xs msg = case RT.findFirstPrefix screenNameSym msg of
                              Nothing      -> False
                              -- Example case: msg == "焼肉か寿司を選べ。" (--> _ == msg)
                              Just ("", _) -> False
                              -- Example case: msg == "@aiya000焼肉か寿司を選べ。"  (--> Just ("@aiya000", "焼肉か寿司を選べ。"))
                              Just (to, _) -> tail to `elem` xs  -- tail remove '@' prefix (ex: "@aiya_gorira" to "aiya_gorira")

      regulateReply :: Text -> Text
      regulateReply = T.replace "@" "reply_to_"

      -- Append one space to between the screen_name and the message
      -- Example: "@aiya000焼肉か寿司を選べ。" -> "@aiya000 焼肉か寿司を選べ。"
      beExplicitReply :: Text -> Text
      beExplicitReply msg = case RT.findFirstPrefix screenNameSym msg of
                                 Nothing -> error $ "Control.GoriraMeCab.beExplicitReply: " ++
                                                    "detected unknowned behavior\n" ++
                                                    "with this message vvv\n\t" ++
                                                    T.unpack msg
                                 Just ("", _)    -> msg
                                 Just (to, msg') -> let to'   = T.pack to
                                                        regex = (to' `T.snoc` ' ') <$ RT.string to'
                                                    in RT.replace regex msg

      -- Generate sentence but ignore reply message
      generateSentenceWithoutReplying :: MonadIO m => [TweetMessage] -> m TweetMessage
      generateSentenceWithoutReplying tweets = do
          sentence <- generateSentence tweets
          return $ regulateReply sentence
