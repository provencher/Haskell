module Main where

test = [True, False, True]
test2 = [True, True, True]
test3 = [False, False, False]

test4 = [test, test2, test3]


andy :: [Bool] -> Bool
andy [a] = a
andy (a:as) = a && andy as  

ory :: [Bool] -> Bool
ory [a] = a
ory (a:as) = a || ory as


zipy :: [a] -> [b] -> [(a,b)]
zipy (a:as) (b:bs) = (a,b): (zipy as bs)
zipy _ _ = []

zipy3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipy3 (a:as) (b:bs) (c:cs) = (a,b,c) : (zipy3 as bs cs)
zipy3 _ _ _ = [] 

concaty :: [[a]] -> [a]
concaty as = [n | a <- as, n <- a]

-- Test cases for zipy
liz = zipy test test2
liz2 = zipy test [True, False]
liz3 = zipy3 test test2 test3

takey :: Integer -> [a] -> [a]
takey 0 _ = []
takey _ [] = []
takey n (a:as) = a : takey (n-1) as

dropy :: Integer -> [a] -> [a]
dropy 0 xs = xs
dropy _ [] = []
dropy n (a:as) = dropy (n-1) as

-- Checks if an input value is part of the input list
elemy :: (Eq a) => a -> [a] -> Bool
elemy _ [] = False
elemy n (a:as)
	| n == a = True
	| otherwise = elemy n as

check = map (elem True) test4

-- Rewriting map
mapy :: (Eq a) => (a->b) -> [a] -> [b]
mapy _ [] = []
mapy foo (a:as) = (foo a) : (mapy foo as)


mac = mapy (+3) [1..10]
mac2 = mapy (\(a,b) -> a+b) [(1,2),(3,4),(5,6)]

{-
zipwithy :: (Eq a) => (a->b->c) -> [a] -> [b] -> [c]
zipwithy _ [] [] = []
zipwithy _ a []  = a
zipwithy _ [] b  = b
zipwithy foo (a:as) (b:bs) = (foo a b) : (zipwithy foo as bs)


l1 = [n | n<-[1..10], even n /= True]
l2 = [m | n<-[1..10], let m = 2*m]
mac3 = zipwithy (\a b -> a+b) l1 l2
-}

invert :: [a] -> [a]
invert []  = []
invert [a] = [a]
invert (a:as) = (invert as) ++ [a]

mac4 = invert [1..10]

-- Counts how many times all 3 elements are equal
count :: [(String, String, String)] -> Integer
count [] = 0
count list = sum [1 | (a,b,c) <- list, a == b, b == c]

-- unique returns the sublist of those elements that appear precisely once
unique :: Eq a => [a] -> [a]
unique [a] = [a]
unique (x:xs) = x : unique [a | a <-xs, a /= x]


data Tree a b = Leaf b | Fork a (Tree a b) (Tree a b)
{-
ass :: [String] -> Tree a String
ass [] = Leaf []
ass (a:as) = cons a (ass as)

cons :: String -> Tree a String -> Tree a String
cons [] (Fork a lst rst) = Fork a lst rst
cons word Leaf []           = Leaf word
cons word (Fork a lst rst)
	| 
-}



-- Function count returns the number of times a given string is a label
countT :: String -> Tree a String -> Integer
countT word (Leaf b) = (if word == b then 1 else 0)
countT word (Fork a lst rst) = (countT word lst) + (countT word rst)


data Pine a = Empty | Pine a (Pine a) (Pine a)

-- function same returns whether two trees have the same shape
same :: Pine a -> Pine a -> Bool
same Empty Empty = True
same Empty (Pine a _ _) = False
same (Pine a _ _) Empty = False
same (Pine _ lst rst) (Pine _ lst' rst') = (same lst lst') && (same rst rst')

-- List of all labels whose string is less than 3
list :: Pine String -> [String]
list (Pine a lst rst)
	| length a <= 3 = a : ((list lst) ++ (list rst))
	| otherwise     = (list lst) ++ (list rst)

-- ins + build work in tandem to build trees
ins :: [String] -> Pine String
ins [] = Empty
ins (a:as) = buildPine a (ins as)

buildPine :: String -> Pine String -> Pine String
buildPine [] (Pine a lst rst) = Pine a lst rst
buildPine word Empty = Pine word Empty Empty
buildPine word (Pine a lst rst)
	| (length word) > (length a) = Pine a lst (buildPine word rst)
	| otherwise                  = Pine a (buildPine word lst) rst

-- Traversal
inOrder :: Pine a -> [a]
inOrder Empty = []
inOrder (Pine a lst rst) = (inOrder lst) ++ [a] ++ (inOrder rst)

-- Tree depth
depthy :: Pine a -> Integer
depthy Empty = 0
depthy (Pine a lst rst) = 1 + max (depthy lst) (depthy rst)


data Entry =Entry {size :: Integer,
			       trav :: [String]}
			     deriving (Eq, Show)


stringList1 = ["a", "aa", "aaaa", "aaa","aaaa", "abc", "aab", "aaaa"]
stringList2 = ["aa", "aaaa", "aaa","aaaa", "abc", "aab", "aaaa", "a"]
stringList3 = ["b", "bb", "aaaa", "aaa","aaaa", "abc", "aab", "aaaa"]


pineList = [(ins stringList1), (ins stringList2), (ins stringList3)]

entryList = [(Entry a b)| pine <- pineList, let a = depthy pine, let b = inOrder pine]

comp = [same (ins stringList1) b| b <- pineList]
