module Routing.Types where

import qualified Data.StrMap as M
import Data.List

data RoutePart = Path String | Query (M.StrMap String)
type Route = List RoutePart
