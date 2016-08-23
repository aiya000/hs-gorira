
-- Module for app configurations

module Data.Config
  ( ConfigTerm (..)
  , GoriraConfig
  ) where

import Data.Map (Map)


-- Config Value Wrapper
data ConfigTerm = TermBool Bool
                deriving (Show, Read)

-- hs-gorira behavior data type
type GoriraConfig = Map String ConfigTerm
