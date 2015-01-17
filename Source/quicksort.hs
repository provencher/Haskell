module Main where


list = [6,4,1,7,3,6,8,3,5,33,5,66,22,44,53,5,3,5,6,7]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort s@(a:as) = 
	let small = [b | b<-s, b < a]
	    big   = [b | b<-s, b > a]
	in quickSort small ++ [a] ++ quickSort big     

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
--mergeSort s@(a:as) =


partition :: [a] -> [[a]]
partition [a]    = [[a]]
partition (a:as) = [a] : partition as


