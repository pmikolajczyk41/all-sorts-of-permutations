module Sorters (
  Sorter,
  insertSort,
  selectSort,
  bubbleSort,
  quickSort,
  mergeSort,
  runCheck,
  sorters,
  testOn
) where

import qualified Data.List as List (delete, splitAt)

type Cmp a = a -> a -> Bool
type Sorter a = Cmp a -> [a] -> [a]

-- INSERTION SORT
insert' :: Cmp a -> a -> [a] -> [a]
insert' _ x [] = [x]
insert' p x yss@(y:ys) =
    if p x y then x : yss else y : insert' p x ys

insertSort :: Sorter a
insertSort _ [] = []
insertSort p (x:xs) = insert' p x (insertSort p xs)

-- SELECTION SORT
minimum' :: Cmp a -> [a] -> a
minimum' _ [x] = x
minimum' p (x:xs) = min p x (minimum' p xs)
    where min p x y = if p x y then x else y

selectSort :: (Eq a) => Sorter a
selectSort _ [] = []
selectSort p xs = least : selectSort p greater
    where least = minimum' p xs
          greater = List.delete least xs

-- BUBBLE SORT
bubble :: Sorter a
bubble _ [] = []
bubble _ [x] = [x]
bubble p (x:xs) = 
    let yss@(y:ys) = bubble p xs
        b = p x y
    in  if b then x : yss else y : x : ys

bubbleSort :: Sorter a
bubbleSort _ [] = []
bubbleSort p xs = least:bubbleSort p greater
    where least:greater = bubble p xs


-- QUICK SORT
quickSort :: Sorter a
quickSort _ [] = []
quickSort p (x:xs) =
    let fp = flip p
        less = filter (fp x) xs
        less' = quickSort p less
        greater = filter (not . fp x) xs
        greater' = quickSort p greater
    in  less' ++ [x] ++ greater'

-- MERGE SORT
merge :: Cmp a -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge p xss@(x:xs) yss@(y:ys)
    |p x y = x : merge p xs yss
    |otherwise = y : merge p xss ys

mergeSort :: Sorter a
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort p xs = merge p leftS rightS
    where half = length xs `div` 2
          (left, right) = List.splitAt half xs
          leftS = mergeSort p left
          rightS = mergeSort p right  

-- TEST
checkSorted :: (a -> a -> Bool) -> [a] -> Bool
checkSorted _ [] = True
checkSorted _ [x] = True
checkSorted p (x:y:z) = p x y && checkSorted p (y:z)

runCheck :: Cmp a -> [Sorter a] -> [a] -> ()
runCheck p ss xs
    |all (checkSorted p) results = ()
    |otherwise = error "Incorrect sorting"
        where results = map (\s -> s p xs) ss

sorters = [insertSort, selectSort, bubbleSort, quickSort, mergeSort]

testOn :: (Eq a) => Cmp a -> [a] -> ()
testOn p xs = runCheck p sorters xs