module Routing.Hash.Aff where

import DOM 
import Control.Monad.Aff
import qualified Routing.Hash as R

modifyHash :: forall e. (String -> String) -> Aff (dom :: DOM|e) Unit 
modifyHash func = makeAff \_ k -> do 
  R.modifyHash func
  k unit

setHash :: forall e. String -> Aff (dom :: DOM|e) Unit 
setHash hash = makeAff \_ k -> do 
  R.setHash hash
  k unit
