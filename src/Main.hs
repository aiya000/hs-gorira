{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraTwitter
import Control.Monad ( forM_ )
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )
import qualified Data.Text.IO as TIO

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
  maybeTimeline <- fetchPublicTimeline oauth credential "aiya_000"
  tlView maybeTimeline

tlView :: Maybe Timeline -> IO ()
tlView Nothing         = fail "connection error"
tlView (Just timeline) = mapM_ (TIO.putStrLn . text) timeline
