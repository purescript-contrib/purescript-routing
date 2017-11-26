module Routing.Hash
  ( getHash
  , setHash
  , modifyHash
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as L
import DOM.HTML.Window (location)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix)

-- | Gets the global location hash.
getHash :: forall eff. Eff (dom :: DOM | eff) String
getHash = window >>= location >>= L.hash >>> map (stripPrefix (Pattern "#") >>> fromMaybe "")

-- | Sets the global location hash.
setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
setHash h = window >>= location >>= L.setHash h

-- | Modifies the global location hash.
modifyHash :: forall eff. (String -> String) -> Eff (dom :: DOM | eff) Unit
modifyHash fn = (fn <$> getHash) >>= setHash
