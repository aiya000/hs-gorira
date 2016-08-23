
-- Module for my exception definitions

module Data.MyException
  ( IOException' (..)
  ) where

import Control.Monad.Catch (Exception)


-- My IOException with String message
data IOException' = IOException' String deriving (Show)
instance Exception IOException'
