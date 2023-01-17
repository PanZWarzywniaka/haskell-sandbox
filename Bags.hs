import Data.List
import Debug.Trace

type Bag a = [(a,Integer)] --deriving Show


index :: Ord a => a -> Bag a -> Maybe Int --return maybe index of element if in a Bag
index el bag =
  case lenghtToEnd el bag of
  Just n -> Just (length bag - n -1)
  Nothing -> Nothing
  where 
    lenghtToEnd :: Ord a => a -> Bag a-> Maybe Int
    lenghtToEnd el [] = Nothing
    lenghtToEnd el ((item,int):xs)
      | el == item = Just (length xs)
      | otherwise = lenghtToEnd el xs

bagInsert :: Ord a => a -> Bag a -> Bag a  --increments number of occurences or inserts new element in front
bagInsert el [] = [(el,1)]
bagInsert el ((x,int):xs)
  | el == x = (x,int+1):xs
  | otherwise = (x,int):bagInsert el xs

bagInsertMultiple :: Ord a => (a,Integer) -> Bag a -> Bag a
bagInsertMultiple (el,int) [] = [(el,int)]
bagInsertMultiple (el,int) ((x,intx):xs)
  | el == x = (x,intx+int):xs
  | otherwise = (x,intx):bagInsertMultiple (el,int) xs


addToBag :: Ord a => [a] -> Bag a -> Bag a --takes list and bag, and returns bag with added items from list
addToBag [] bag = bag
addToBag (x:xs) bag = addToBag xs (bagInsert x bag)

listToBag :: Ord a => [a] -> Bag a --takes list and returns created bag
listToBag list = addToBag list []


bagEqual :: Ord a => Bag a -> Bag a -> Bool
bagEqual a b = equal (sort a) (sort b)
  where
    equal :: Ord a => Bag a -> Bag a -> Bool
    equal [] [] = True
    equal [] y = False
    equal x [] = False
    equal ((elx,intx):xs) ((ely,inty):ys) = elx == ely && intx==inty && bagEqual xs ys

bagSum ::  (Ord a,Show a) => Bag a -> Bag a -> Bag a -- union of two bags
bagSum a b = sum (sort a) (sort b)
  where
    sum :: (Ord a,Show a) => Bag a -> Bag a -> Bag a --already gets sorted elements --inserts elements from x to y
    sum [] bag = bag
    sum (x:xs) bag = sum xs (bagInsertMultiple x bag)

bagIntersection :: (Ord a) => Bag a -> Bag a -> Bag a
bagIntersection a b = intersection (sort a) (sort b)
  where
    intersection :: (Ord a) => Bag a -> Bag a -> Bag a
    intersection [] bag = []
    intersection (x:xs) bag = getIntersection x bag ++ intersection xs bag --in each call gets intersection and adds it to the rest
      where
        getIntersection :: Ord a => (a,Integer) -> Bag a -> Bag a --intersection between
        getIntersection (el,int) [] = []
        getIntersection (el,int) ((x,intx):xs)
          | el == x = [(x,min int intx)] --we found element
          | otherwise = getIntersection (el,int) xs

test = bagIntersection (listToBag ["a","a", "b", "c"]) (listToBag ["a","a","a","b"])