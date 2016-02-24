module Routing.Hash where

import Prelude (Unit, (<$>), (>>=))
import Control.Monad.Eff (Eff())
import DOM (DOM())

foreign import setHash :: forall e. String -> Eff (dom :: DOM |e) Unit

foreign import getHash :: forall e. Eff (dom :: DOM |e) String

modifyHash :: forall e. (String -> String) -> Eff (dom :: DOM|e) Unit
modifyHash fn = (fn <$> getHash) >>= setHash
