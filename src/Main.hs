{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraMeCab
import Control.GoriraTwitter
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )

-- temporary
import Data.Maybe ( fromJust )
import Control.Monad ( forM_ )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
--

newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

main :: IO ()
main = do
  oauth         <- readOAuth
  accessTokens  <- readAccessTokens
  let credential = newCredential' accessTokens
  maybeTimeline <- fetchUserTimeline oauth credential "aiya_000"
  tweetMessage <- generateTweet $ fromJust maybeTimeline
  postTweet oauth credential tweetMessage
