module Predicates (
  coinCmp,
  consistentCoinCmp,
  Choices,
  CmpMS,
  noChoices,
  checkChoices,
  storeChoice,
) where

import Control.Monad.State
import Sorters (Cmp)
import SortersM (CmpM, SorterM)

-- COMMON COIN

coinCmp :: (MonadPlus m) => CmpM a m
coinCmp _ _ = return True `mplus` return False

-- CONSISTENT COIN

type Choices a = [((a, a), Bool)]
type CmpMS a m = CmpM a (StateT (Choices a) m)

noChoices :: Choices a
noChoices = []

addChoice :: a -> a -> Bool -> Choices a -> Choices a
addChoice x y b = (((x, y), b):)

storeChoice :: (Eq a, MonadPlus m) => (Choices a -> Choices a) -> CmpM a m -> CmpMS a m
storeChoice update p x y = do
    b <- lift (p x y)
    modify (update . addChoice x y b)
    return b

checkChoices :: (Eq a, MonadPlus m) => CmpMS a m -> CmpMS a m
checkChoices p x y = do
    s <- get
    maybe (p x y) return (lookup (x, y) s)
    
consistentCoinCmp :: (Eq a, MonadPlus m) => CmpMS a m
consistentCoinCmp = checkChoices $ storeChoice id coinCmp
