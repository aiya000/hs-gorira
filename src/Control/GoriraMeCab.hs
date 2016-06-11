{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraMeCab
  ( generateTweet
  ) where

import Data.GoriraMeCab
import Data.GoriraTwitter
import Data.List ( concat, foldl1 )
import Data.Text ( Text () )
import Prelude hiding ( foldl1 )
import System.Random.Shuffle ( shuffleM )
import Text.MeCab ( new, parseToNodes, Node (..) )


generateTweet :: Timeline -> IO TweetMessage
generateTweet timeline = do
  let tweets = map text timeline
  mecab <- new ["mecab"]
  nodes <- mapM (parseToNodes mecab) tweets
  let sentences = map (toChainable . filter (/= "") . map nodeSurface) $ nodes
  sentences' <- shuffleM . concat $ sentences
  let tweetMessage = chainWords sentences'
  return tweetMessage

toChainable :: [Text] -> [ChainableWords]
toChainable [x] = [ChainableWords (Begin x) (End "")]
toChainable xs  = zipWith ChainableWords xs' (tail xs')
  where
    beginWord   = Begin . head $ xs
    endWord     = End   . last $ xs
    middleWords = map Middle . init . tail $ xs
    xs'         = [beginWord] ++ middleWords ++ [endWord]

chainWords :: [ChainableWords] -> Text
chainWords []  = error "aho-baka-hoge"
chainWords [x] = toText x
chainWords xs  = toText . foldl1 chain . dropWhile (not . hasBegin) $ xs
  where
    chain :: ChainableWords -> ChainableWords -> ChainableWords
    chain a@(ChainableWords _ y1) b@(ChainableWords _ y2)
      | areChainable a b = if isEnd y1
                              then a
                              else let l = Begin . toText $ a
                                   in ChainableWords l y2
      | otherwise = a
