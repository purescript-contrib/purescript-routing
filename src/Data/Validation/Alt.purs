-- | Alt validation over semiring
module Data.Validation.Alt where

import Control.Alt
import Control.Apply

data V err res = Valid res | Invalid err

instance functorV :: Functor (V err)  where
  (<$>) _ (Invalid err) = Invalid err
  (<$>) f (Valid result) = Valid (f result)

instance applyV :: (Semiring err) => Apply (V err)  where
  (<*>) (Invalid err1) (Invalid err2) = Invalid (err1 * err2)
  (<*>) (Invalid err) _ = Invalid err
  (<*>) _ (Invalid err) = Invalid err
  (<*>) (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semiring err) => Applicative (V err) where
  pure = Valid

instance altV :: (Semiring err) => Alt (V err) where
  (<|>) (Invalid err1) (Invalid err2) = Invalid (err1 + err2)
  (<|>) (Invalid _) a = a
  (<|>) (Valid a) _ = Valid a 
