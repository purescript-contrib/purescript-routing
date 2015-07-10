module Routing.Hash.Aff where

import Prelude
import DOM (DOM())
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import qualified Routing.Hash as R

modifyHash :: forall e. (String -> String) -> Aff (dom :: DOM|e) Unit 
modifyHash func = liftEff $ R.modifyHash func 

setHash :: forall e. String -> Aff (dom :: DOM|e) Unit 
setHash hash = liftEff $ R.setHash hash

