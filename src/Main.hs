{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraTwitter
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )

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
  maybeTimeline <- fetchPublicTimeline oauth credential
  dummy maybeTimeline
  return ()

dummy :: Maybe HomeTimeline -> IO ()
dummy Nothing         = putStrLn "dame"
dummy (Just timeline) = print timeline
