{-# LANGUAGE OverloadedStrings #-}
module Control.GoriraMeCab
  ( generateTweet
  ) where
import Data.GoriraTwitter
import Data.List ( concatMap, concat, foldl1 )
import Data.List ( reverse )
import Data.Text ( Text () )
import Prelude hiding ( foldl1 )
import System.Random.Shuffle ( shuffleM )
import Text.MeCab ( new, parseToNodes, Node (..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data PositionText = Begin Text | Middle Text | End Text deriving (Show)

extractPositionText :: PositionText -> Text
extractPositionText (Begin  x) = x
extractPositionText (Middle x) = x
extractPositionText (End    x) = x

isEnd :: PositionText -> Bool
isEnd (End _) = True
isEnd _       = False

type PositionTextCons = (Text -> PositionText)
getPosition :: PositionText -> PositionTextCons
getPosition (Begin _)  = Begin
getPosition (Middle _) = Middle
getPosition (End _)    = End


data ChainableWords = ChainableWords PositionText PositionText deriving (Show)

areChainable :: ChainableWords -> ChainableWords -> Bool
areChainable (ChainableWords _ y1) (ChainableWords x2 _) =
  let jointL = extractPositionText y1
      jointR = extractPositionText x2
  in jointL == jointR

toText :: ChainableWords -> Text
toText (ChainableWords x y) = extractPositionText x `T.append` extractPositionText y

hasBegin :: ChainableWords -> Bool
hasBegin (ChainableWords (Begin _) _) = True
hasBegin _                            = False


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
chainWords []  = error "aho"
chainWords [x] = toText x
chainWords xs  = toText . foldl1 chain . dropWhile hasBegin $ xs
  where
    chain :: ChainableWords -> ChainableWords -> ChainableWords
    chain a@(ChainableWords _ y1) b@(ChainableWords _ y2)
      | areChainable a b = if isEnd y1
                              then a
                              else let l = Begin . toText $ a
                                   in ChainableWords l y2
      | otherwise = a
