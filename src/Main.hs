{-# LANGUAGE OverloadedStrings #-}
module Main where
import Config
import Control.GoriraTwitter
import Data.GoriraTwitter
import Web.Authenticate.OAuth ( Credential (), newCredential )
import qualified Data.Text.IO as TIO
import Control.Monad ( forM_ )

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
  return ()

tlView :: Maybe Timeline -> IO ()
tlView Nothing         = putStrLn "dame"
tlView (Just timeline) = do
  forM_ timeline $ \t -> do
    TIO.putStrLn . text $ t
