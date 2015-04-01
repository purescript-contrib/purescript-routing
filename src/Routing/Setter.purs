module Routing.Setter where

import Control.Monad.Eff

foreign import setHash """
function setHash(hash) {
  return function() {
    document.location.hash = hash;
  };
}
""" :: forall e. String -> Eff e Unit

foreign import getHash """
function getHash() {
  return document.location.hash.replace(/^[^#]*#/g, "");
}
""" :: forall e. Eff e String

class RouteModifier a where
  toHashModifier :: a -> (String -> String) 

-- | Class of types that can be converted to hashes 
class RouteState a where
  toHash :: a -> String

-- | wrapper over `setHash` that uses `RouteState`
setRouteState :: forall r e. (RouteState r) => r -> Eff e Unit
setRouteState r = setHash $ toHash r


modifyRoute :: forall r e. (RouteModifier r) => r -> Eff e Unit
modifyRoute r = (toHashModifier r <$> getHash) >>= setHash 
