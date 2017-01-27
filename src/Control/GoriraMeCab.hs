{-# LANGUAGE OverloadedStrings #-}

-- This module for generating tweet message

module Control.GoriraMeCab
  ( generateTweet
  ) where

import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Sentence.Japanese (generateMessage, GenerateOption (IgnoreSigns, IgnoreAlphaNums))
import Data.Maybe (fromJust)
import Data.MyException
import Data.Text (Text)
import Data.TwiHigh
import Text.Regex.Applicative.Text (RE', (<|>))
import qualified Data.Text as T
import qualified Data.TwiHigh.Followers as Followers
import qualified Text.Regex.Applicative.Text as RT
import qualified Text.Regex.Posix as Regex

-- Temporary fake name in this source
type TwitterScreenName' = String

sentenceOptions :: [GenerateOption]
sentenceOptions = [IgnoreSigns, IgnoreAlphaNums]


-- Generate sentence for twitter tweet
generateTweet :: (MonadCatch m, MonadIO m) => TwitterAuth -> [TweetMessage] -> Bool -> m TweetMessage
generateTweet auth tweets allowReply
  | allowReply = generateSentenceWithFilter auth tweets
  | otherwise  = generateSentenceWithoutReplying tweets
    where
      -- Generate sentence but ignore reply to not followers
      generateSentenceWithFilter :: (MonadCatch m, MonadIO m) => TwitterAuth -> [TweetMessage] -> m TweetMessage
      generateSentenceWithFilter auth tweets = do
        maybeScreenNames <- liftIO $ fmap (map Followers.listUsersScreenName . Followers.listUsers) <$> Followers.fetchFollowersList auth "aiya_gorira"
        sentence         <- liftIO $ generateMessage sentenceOptions tweets
        case sentence of
          Left  e -> throwM $ MessageGenerationException $ "message generating is failure: " ++ e
          Right a -> if isReplyTweet a
            then return a
            else case isReplyTo <$> maybeScreenNames <*> (Just a) of
                      Nothing    -> throwM $ IOException' "fetching followers list was failed"
                      Just True  -> return $ beExplicitReply a
                      Just False -> return $ regulateReply . beExplicitReply $ a

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
      generateSentenceWithoutReplying :: (MonadCatch m, MonadIO m) => [TweetMessage] -> m TweetMessage
      generateSentenceWithoutReplying tweets = do
        sentence <- liftIO $ generateMessage sentenceOptions tweets
        case sentence of
          Left  e -> throwM $ MessageGenerationException $ "message generating is failure: " ++ e
          Right a -> return $ regulateReply a
