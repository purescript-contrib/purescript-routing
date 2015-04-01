module Routing.Hash where

import Control.Monad.Eff
import DOM

foreign import setHash """
function setHash(hash) {
  return function() {
    document.location.hash = hash;
  };
}
""" :: forall e. String -> Eff (dom :: DOM |e) Unit

foreign import getHash """
function getHash() {
  return document.location.hash.replace(/^[^#]*#/g, "");
}
""" :: forall e. Eff (dom :: DOM |e) String

modifyHash :: forall e. (String -> String) -> Eff (dom :: DOM|e) Unit
modifyHash fn = (fn <$> getHash) >>= setHash 
