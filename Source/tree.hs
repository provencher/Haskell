--import Data.List

module Main where

kmin = 2  -- how big do you want your trees to be
kmax = 5

sizes = [3,7,15,31]  -- tree sizes of trees you will create

data Tree = Empty | Tree Integer Tree Tree
{-
make :: Integer -> Integer -> Tree  -- turn contiguous sequence j..k into tree
make j k
  | ...

  | ...

    where ...
-}
{-
end :: [a] -> a
end []     = []
end [a]    = [a]
end (a:as) = last as

--from Data.List
ini :: (Ord a) => [a] -> [a]
ini as = [a | a <- as, a /= (end as)]
-}

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

getMid :: [a] -> a
getMid as = 
	let (b:bs) = middle as
	in b


-- build tree 
turn :: [Integer] -> Tree
turn [] = Empty
turn l@(a:as) = 
	let mid  = getMid l	    
	    small= [num | num <- l, num < mid]
	    big  = [num | num <- l, num > mid]	    

	in  Tree mid (turn small) (turn big)

inorder :: Tree -> [Integer]  -- inorder traversal
inorder Empty = []
inorder (Tree a lst rst) = (inorder lst) ++ [a] ++ (inorder rst)

depth' :: Tree -> Integer  -- depth of tree
depth' Empty = 0
depth' (Tree a lst rst) = 1 + max (depth' lst) (depth' rst) 

treesPlus = [(tree, n) | n <- sizes, let tree = turn [1..n]] -- list of pairs of trees and their sizes

data Record = Record {size  :: Integer,
                      inOrd :: [Integer],
                      depth :: Integer}
              deriving (Eq, Show)

records = [(Record b (inorder a) (depth' a)) | (a,b) <- treesPlus]  -- one for every tree


main = do
  print sizes
  putStrLn $ unlines $ map show records


{-
[3,7,15,31]
Record {size =  3, inOrd = [1,2,3],                depth = 2}
Record {size =  7, inOrd = [1,2,3,4,5,6,7],        depth = 3}
Record {size = 15, inOrd = [1,2,3,4,5,6,7,8,9,10], depth = 4}
Record {size = 31, inOrd = [1,2,3,4,5,6,7,8,9,10], depth = 5}
-}

{- I bounded the printout of 'inOrd' to 10 elements max -}
