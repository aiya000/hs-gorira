{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraMeCab
  ( generateTweet
  ) where
import Data.GoriraTwitter
import Data.List ( concatMap, concat )
import Data.List ( reverse )
import Data.Text ( Text () )
import System.Random.Shuffle ( shuffleM )
import Text.MeCab ( new, parseToNodes, Node (..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data ChainableWords = ChainableWords Text (Bool, Text)


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
toChainable [x] = []  -- to dustbox
toChainable xs  = (init body) ++ [end]
  where
    body = zipWith (\x y -> ChainableWords x (False, y)) xs (drop 1 xs)
    end  = (\[x, y] -> ChainableWords x (True, y)) . reverse . take 2 . reverse $ xs

chainWords :: [ChainableWords] -> Text
chainWords [] = undefined -- you have done tweet ?
chainWords ((ChainableWords w (_, v)) : xs) = T.concat $ chain [w, v] xs
  where
    chain :: [Text] -> [ChainableWords] -> [Text]
    chain ys [] = if ys == [""] then ["(^o^)< empty tweet!"] else ys
    chain ys ((ChainableWords a (True,  b)) : zs) =
      if (last ys) == a then ys ++ [b]
                        else (chain ys zs)
    chain ys ((ChainableWords a (False,  b)) : zs) =
      if (last ys) == a then ys ++ (chain [b] zs)
                        else (chain ys zs)
