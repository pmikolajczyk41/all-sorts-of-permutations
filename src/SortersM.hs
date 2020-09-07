module SortersM (
  SorterM,
  CmpM,
  insertSortM,
  selectSortM,
  bubbleSortM,
  quickSortM,
  mergeSortM,
  runCheckM,
  sortersM,
  testOnM
) where

import Control.Monad
import Control.Monad.Identity
import qualified Data.List as List (delete, splitAt)

type CmpM a m = a -> a -> m Bool
type SorterM a m = CmpM a m -> [a] -> m [a]

-- INSERTION SORT
insertM :: (Monad m) => CmpM a m -> a -> [a] -> m [a]
insertM _ x [] = return [x]
insertM p x yss@(y:ys) = do
    b <- p x y
    if b then return (x:yss)
         else fmap (y:) (insertM p x ys)

insertSortM :: (Monad m) => SorterM a m
insertSortM _ [] = return []
insertSortM p (x:xs) = do
    ys <- insertSortM p xs
    insertM p x ys

-- SELECTION SORT
minM :: (Monad m) => CmpM a m -> a -> a -> m a
minM p x y = do
    b <- p x y
    return (if b then x else y)

minimumM :: (Monad m) => CmpM a m -> [a] -> m a
minimumM _ [x] = return x
minimumM p (x:xs) = do
    y <- minimumM p xs
    minM p x y

selectSortM :: (Eq a, Monad m) => SorterM a m
selectSortM _ [] = return []
selectSortM p xs = do
    x <- minimumM p xs
    fmap (x:) (selectSortM p (List.delete x xs))

-- BUBBLE SORT
bubbleM :: (Monad m) => CmpM a m -> [a] -> m [a]
bubbleM _ [] = return []
bubbleM _ [x] = return [x]
bubbleM p (x:xs) = do
    yss@(y:ys) <- bubbleM p xs
    b <- p x y
    return (if b then x : yss else y : x : ys)

bubbleSortM :: (Monad m) => SorterM a m
bubbleSortM _ [] = return []
bubbleSortM p xs = do
    (y:ys) <- bubbleM p xs
    fmap (y:) (bubbleSortM p ys)

-- QUICK SORT
quickSortM :: (Monad m) => SorterM a m
quickSortM _ [] = return []
quickSortM p (x:xs) = do
    let fp = flip p
    less <- filterM (fp x) xs
    less' <- quickSortM p less
    greater <- filterM (fmap not . fp x) xs
    greater' <- quickSortM p greater
    return (less' ++ [x] ++ greater')

-- MERGE SORT
mergeM :: (Monad m) => CmpM a m -> [a] -> [a] -> m [a]
mergeM _ [] ys = return ys
mergeM _ xs [] = return xs
mergeM p xss@(x:xs) yss@(y:ys) = do
    b <- p x y
    if b then fmap (x:) (mergeM p xs yss)
         else fmap (y:) (mergeM p xss ys)

mergeSortM :: (Monad m) => SorterM a m
mergeSortM _ [] = return []
mergeSortM _ [x] = return [x]
mergeSortM p xs = do
    let half = length xs `div` 2
        (left, right) = List.splitAt half xs
    leftS <- mergeSortM p left
    rightS <- mergeSortM p right
    mergeM p leftS rightS

-- TESTING
runCheckM :: (a -> a -> Bool) -> SorterM a Identity -> [a] -> Bool
runCheckM p s xs
    |checkSorted p $ runIdentity $ s (\x y -> return $ p x y) xs = True
    |otherwise = error "Incorect monadic lift"
    where checkSorted p xs = all (uncurry p) (zip xs (tail xs))
    
sortersM = [insertSortM, selectSortM, bubbleSortM, quickSortM, mergeSortM]

testOnM :: (Eq a) => (a -> a -> Bool) -> [a] -> String
testOnM p xs = if all (\s -> runCheckM p s xs) sortersM then "OK" else "FAIL"