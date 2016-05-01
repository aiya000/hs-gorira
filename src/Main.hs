{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraTwitter
import Control.Monad.Trans.Resource ( runResourceT )
import Web.Authenticate.OAuth ( Credential (), newCredential )

newCredential' :: TwitterAccessTokens -> Credential
newCredential' accessTokens =
  let accessTokenValue       = accessToken accessTokens
      accessTokenSecretValue = accessTokenSecret accessTokens
  in newCredential accessTokenValue accessTokenSecretValue

main :: IO ()
main = do
  oauth        <- readOAuth
  accessTokens <- readAccessTokens
  let credential = newCredential' accessTokens
  runResourceT $ tweet "Hello, world!! from Haskell" oauth credential
