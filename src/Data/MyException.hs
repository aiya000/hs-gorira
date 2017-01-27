
-- Module for my exception definitions

module Data.MyException
  ( IOException' (..)
  , MessageGenerationException (..)
  ) where

import Control.Monad.Catch (Exception)


-- My IOException with String message
data IOException' = IOException' String deriving (Show)
instance Exception IOException'

-- A exception of pure context
data MessageGenerationException = MessageGenerationException String deriving (Show)
instance Exception MessageGenerationException
