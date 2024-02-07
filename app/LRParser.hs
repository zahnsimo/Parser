module LRParser where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (map, filter)--as Map
import Control.Monad.Reader
import Data.List hiding (lookup)
import Data.Maybe

import Main

newtype Item = Item (NT, ([RuleChar], [RuleChar]) )

type AGTable = Map (Int, RuleChar)
