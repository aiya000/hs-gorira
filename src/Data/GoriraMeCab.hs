module Data.GoriraMeCab
  ( PositionText (..)
  , extractPositionText
  , isEnd
  , getPosition
  , ChainableWords (..)
  , areChainable
  , toText
  , hasBegin
  ) where

import Data.Text ( Text () )
import qualified Data.Text as T

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
getPosition (Begin  _) = Begin
getPosition (Middle _) = Middle
getPosition (End    _) = End


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
