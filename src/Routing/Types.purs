module Routing.Types where

import Data.List (List)
import Data.Map (Map)
import Prelude (class Eq)

data RoutePart = Path String | Query (Map String String)

derive instance eqRoutePart :: Eq RoutePart

type Route = List RoutePart
