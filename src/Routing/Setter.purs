module Routing.Setter where

import Control.Monad.Eff

foreign import setHash """
function setHash(hash) {
  return function() {
    document.location.hash = hash;
  };
}
""" :: forall e. String -> Eff e Unit

class RouteState a where
  toHash :: a -> String

setRouteState :: forall r e. (RouteState r) => r -> Eff e Unit
setRouteState r = setHash $ toHash r
