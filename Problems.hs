-- 99 Problems

import Data.List

--
-- 01 Find the last element of a list.
--

myLast [] = error "No end for an empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

--
-- 02 Find the last but one element of a list.
--

myButLast [] = error "No last but one for an empty list"
myButLast [x] = error "No last but one for a 1 element list"
myButLast [x,_] = x 
myButLast (x:xs) = myButLast xs

--
-- 03 Find the K'th element of a list. The first element in the list is number 1.
--

elementAt _ 0 = error "Index below bounds"
elementAt [] _ = error "Index beyond bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) position = elementAt xs (position - 1)

--
-- 04 Find the number of elements of a list.
--

myLengthCounter [] count = count
myLengthCounter (x:xs) count = myLengthCounter xs (count + 1)

myLength list = myLengthCounter list 0 

-- Simpler version

myLength' [] = 0
myLength' (x:xs) = 1 + myLength xs

--
-- 05 Reverse a list.
--

-- Inefficient

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Efficient

myReverse' :: [a] -> [a]
myReverse' list = myReverse'' list []
    where
        myReverse'' [] reversed = reversed
        myReverse'' (x:xs) reversed = myReverse'' xs (x:reversed)

--
-- 06 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
--

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome list = list == reversed
    where
        reversed = myReverse' list

--
-- 07 Flatten a nested list structure.
--

--data NestedList a = Elem a | List [NestedList a]

--flatten :: NestedList a -> [a]
--flatten (Elem x) = [x]
--flatten (List x) = concatMap flatten x

--
-- 08 Eliminate consecutive duplicates of list elements.
--

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)

--
-- 09 Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
--

pack :: (Eq a) => [a] -> [[a]]
pack = group -- Lol

--
-- 10 Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
--

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map runLength (group xs)
    where runLength list = (length list, head list)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . group

--
-- 11 Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.
--

data Item a = Single a | Multiple Int a
    deriving (Show)

encodeModified xs = map modRunLength (group xs) 
        where 
    modRunLength list
        | length list == 1 = Single (head list)
        | otherwise = Multiple (length list) (head list)

--
-- 12 Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--

decodeModified :: [Item a] -> [a]
decodeModified list = concatMap runLengthToList list
        where
    runLengthToList (Single x) = [x]
    runLengthToList (Multiple count x) = replicate count x

--
-- 14 Duplicate the elements of a list.
--

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

--
-- 15 Replicate the elements of a list a given number of times.
--

repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs

--
-- 16 Drop every N'th element from a list.
--

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryRec xs n n
    where
        dropEveryRec [] _ _ = []
        dropEveryRec (x:xs) 1 n = dropEveryRec xs n n
        dropEveryRec (x:xs) c n = x : dropEveryRec xs (c - 1) n

--
-- 17 Split a list into two parts; the length of the first part is given.
--

split :: [a] -> Int -> ([a], [a])
split xs n = splitRec n [] xs
    where
        splitRec 0 xs ys = (xs, ys)
        splitRec n xs (y:ys) =  splitRec (n - 1) (xs ++ [y]) ys

--
-- 18 Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1.
--

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j + 1 - i) $ drop (i - 1) xs

--
-- 19 Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
--

rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0    = drop n xs ++ take n xs
    | otherwise = drop k xs ++ take k xs
        where k = length xs + n 