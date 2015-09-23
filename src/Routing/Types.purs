module Routing.Types where

import Data.List (List())
import Data.Map (Map())

data RoutePart = Path String | Query (Map String String)
type Route = List RoutePart
