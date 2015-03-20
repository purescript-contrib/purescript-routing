module Routing.Match.Error where

import Data.Monoid
import Data.Traversable 
import Data.Array
import Control.Monad.Error 

newtype RoutingError = RoutingError [[String]]

orRE :: RoutingError -> RoutingError -> RoutingError
orRE (RoutingError errs1) (RoutingError errs2) =
  RoutingError $ errs1 <> errs2

zeroRE :: RoutingError
zeroRE = RoutingError []

andRE :: RoutingError -> RoutingError -> RoutingError
andRE (RoutingError errs1) (RoutingError errs2) =
  RoutingError $ do
    e1 <- errs1
    e2 <- errs2
    pure $ e1 <> e2

oneRE :: RoutingError
oneRE = RoutingError [[]]


instance routingErrorSemigroup :: Semigroup RoutingError  where
  (<>) = andRE 
instance routingErrorMonoid :: Monoid RoutingError where
  mempty = oneRE

instance routingErrorSemiring :: Semiring RoutingError where
  one = oneRE
  zero = zeroRE
  (+) = orRE
  (*) = andRE

instance routingErrorError :: Error RoutingError where
  noMsg = zeroRE
  strMsg msg = RoutingError [[msg]]
