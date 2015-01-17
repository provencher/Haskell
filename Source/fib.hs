
module Main where

import System.Info
import System.IO
import Data.Char


fib 0 a b = a	-- where a and b have to be 0 and 1
fib n a b = fib (n - 1) b (a + b)

fibonacci n = fib n 0 1

r = 100

--turns int to a String
stringInt :: Integer-> String
stringInt a = show a

--puts all fibs from 0 to a certain integer into a list of Strings ++ \n
fibListMaker :: Integer -> [String]
fibListMaker y = [(stringInt (fibonacci n)) ++ "\n" | n <- [0..y]]


--list = [(stringInt (fibonacci n)) ++ "\n" | n <- [0..r]]

blankline = putStrLn "\n"

--Takes list of Strings and creates a single String out of all of them
listString :: [String] -> String
listString [] = []
listString xs = (head xs) ++ listString(tail xs)

--myString = listString list


main = do
	putStrLn "Enter a number:"
	inNum <- readLn
	putStrLn "The fibonacci is"
	print $ fibonacci inNum

	putStrLn "\nNow I will calculate all fibonaccis of 0 to your number"
	--putStrln list
	putStrLn (listString (fibListMaker inNum))
	putStrLn "Complete"
	
{-

Enter a number:
12
The fibonacci is
144

Now I will calculate all fibonaccis of 0 to your number
0
1
1
2
3
5
8
13
21
34
55
89
144

Complete

-}

