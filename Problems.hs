-- 99 Problems

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

data NestedList a = Elem a | List [NestedList a]

flatten [] = []
--flatten (x:xs) = 