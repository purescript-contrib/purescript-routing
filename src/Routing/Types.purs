module Routing.Types where

import qualified Data.StrMap as M

data RoutePart = Path String | Query (M.StrMap String)
type Route = [RoutePart]
