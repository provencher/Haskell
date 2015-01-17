module Main where

n = 8               -- total number of ingredients available

--xor implementation
xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

xor'' :: Bool -> Bool -> Bool
xor'' True a = not a
xor'' False a = a

type Recipe = [Int] -- subset of these ingredients

-- all k-ingredient recipes given n possible ingredients 1..n

choose :: Int -> Int -> [Recipe]  -- don't mess with this
choose n k
  | k == 0    = [[]]      -- only one recipe with no ingredients

  | n == k    = [[1..n]]  -- only one recipe with all ingredients

  | otherwise = choose (n-1) k ++ map (++ [n])(reverse (choose (n-1) (k-1)))

                          -- black magic: take on faith (it's combinatorics)
                          
                          


-- rules for legal omlettes (must hold conjunctively)

ruleset = [rule0, rule1, rule2, rule3, rule4, rule5, rule6, rule7]

-- Rule 0: If truffles, then precisely truffles.
rule0 r = (preciselyTruffles r) || not(truffles r)

-- Rule 1: Either truffles or garlic or some meat.
rule1 r = (xor ((xor (truffles r) (garlic r))) ((sausage r) || (ham r) || (bacon r)))


-- Rule 2: Not both peppers and onions.
rule2 r = not ((peppers r) && (onions r))

-- Rule 3: Not both garlic and onions.
rule3 r = not ((garlic r) && (onions r))

-- Rule 4: Not both garlic and peppers.
rule4 r = not ((garlic r) && (peppers r))

-- Rule 5: If bacon, then peppers.
rule5 r = (peppers r) || not (bacon r)

-- Rule 6: If sausage, then onions.
rule6 r = (onions r) || not (bacon r)

-- Rule 7: If ham, then mushrooms.
rule7 r = (mushrooms r) || not (ham r)

-- ingredient encoding (don't mess with this)

bacon     = elem 1

peppers   = elem 2

ham       = elem 3

mushrooms = elem 4

sausage   = elem 5

onions    = elem 6

garlic    = elem 7

truffles  = elem 8

preciselyTruffles = (== [8])

names =
  ["bacon ", "peppers ", "ham ", "mushrooms ", "sausage ", "onions ", "garlic ", "truffles "]

ingredient_costs = [5, 6, 5, 7, 8, 6, 10, 35]

-- main processing

legal :: Recipe -> Bool
legal r = and [rule r | rule <- ruleset] -- for each rule in ruleset, apply rule on r, which is recipe 


cost :: Recipe -> Int  -- given the ingredient costs, determine the recipe cost
cost r = sum[ingredient_costs!!(j-1) | j <- r]


tabulate :: [Recipe] -> [Int]    -- given a set of recipes, find the ingredient
tabulate rs = [length $ filter (==i)(concat rs) | i<- [1..n]] -- histogram of the whole set
--The above histogram generator uses the filter and concat functions from prelude, and does the following:
--It returns the length of each list returned by the loop that filters through each of the desired elements of a concatenated list
--The returned length represents the number of elements in each filtered list


-- print routines

printRecipes :: [Recipe] -> IO ()
printRecipes = putStrLn . unlines . map show

showChoices :: Int -> IO ()      -- this is a monad (more black magic)
showChoices k = do
  blank
  blank
  putStrLn $ "Choices with " ++ show k ++ " Ingredient(s)" ++ tab2 ++ "  Cst    Prc"
  putStrLn $ "____________________________" ++ tab2 ++ "  ___    ___"
  blank
  let legalRecipes = filter legal $ choose n k
  showRecipes legalRecipes
  putStr "Ingredient Histogram: "
  let needs = tabulate legalRecipes
  print needs
  putStrLn caption

showRecipes :: [Recipe] -> IO ()
showRecipes = putStrLn . unlines . map showNames

showNames :: Recipe -> String  -- determine a string that displays the recipe in English
showNames r = let
                str = concat $ "eggs " : [names!!(j-1) | j <- r] -- As names is a list of all ingredients, this line loops the given recipe, associates each ingredient with an index in names, and then concatenates a string of available ingredients
              in
                str ++ replicate (40 - length str) ' ' ++
                  (if garlic r then "Complimentary Breath Mints" else tab1) ++
                    ("   $" ++ show (cost r)) ++
                      (if truffles r then "    $80" else "    $40")

blank = putStrLn ""

tab1 = replicate 26 ' '

tab2 = replicate 39 ' '

caption  = "                       b p h m s o g t"

main = do
  blank
  putStrLn "First Ten 4-Ingredient Digital Recipes"
  putStrLn "______________________________________"
  blank
  printRecipes $ take 10 $ choose n 4
  putStrLn "First Ten 4-Ingredient English Recipes"
  putStrLn "______________________________________"
  blank
  showRecipes $ take 10 $ choose n 4
  showChoices 4
  showChoices 3
  showChoices 2
  showChoices 1
  blank
  
  
{-

--OUTPUT

  First Ten 4-Ingredient Digital Recipes
______________________________________

[1,2,3,4]
[1,2,4,5]
[2,3,4,5]
[1,3,4,5]
[1,2,3,5]
[1,2,5,6]
[2,3,5,6]
[1,3,5,6]
[3,4,5,6]
[2,4,5,6]

First Ten 4-Ingredient English Recipes
______________________________________

eggs bacon peppers ham mushrooms                                     $23    $40
eggs bacon peppers mushrooms sausage                                 $26    $40
eggs peppers ham mushrooms sausage                                   $26    $40
eggs bacon ham mushrooms sausage                                     $25    $40
eggs bacon peppers ham sausage                                       $24    $40
eggs bacon peppers sausage onions                                    $25    $40
eggs peppers ham sausage onions                                      $25    $40
eggs bacon ham sausage onions                                        $24    $40
eggs ham mushrooms sausage onions                                    $26    $40
eggs peppers mushrooms sausage onions                                $27    $40



Choices with 4 Ingredient(s)                                         Cst    Prc
____________________________                                         ___    ___

eggs peppers ham mushrooms sausage                                   $26    $40
eggs ham mushrooms sausage onions                                    $26    $40

Ingredient Histogram: [0,1,2,2,2,1,0,0]
                       b p h m s o g t


Choices with 3 Ingredient(s)                                         Cst    Prc
____________________________                                         ___    ___

eggs peppers ham mushrooms                                           $18    $40
eggs peppers mushrooms sausage                                       $21    $40
eggs ham mushrooms sausage                                           $20    $40
eggs mushrooms sausage onions                                        $21    $40
eggs ham mushrooms onions                                            $18    $40

Ingredient Histogram: [0,2,3,5,3,2,0,0]
                       b p h m s o g t


Choices with 2 Ingredient(s)                                         Cst    Prc
____________________________                                         ___    ___

eggs ham mushrooms                                                   $12    $40
eggs mushrooms sausage                                               $15    $40
eggs peppers sausage                                                 $14    $40
eggs sausage onions                                                  $14    $40
eggs mushrooms garlic                   Complimentary Breath Mints   $17    $40

Ingredient Histogram: [0,1,1,3,3,1,1,0]
                       b p h m s o g t


Choices with 1 Ingredient(s)                                         Cst    Prc
____________________________                                         ___    ___

eggs sausage                                                         $8    $40
eggs garlic                             Complimentary Breath Mints   $10    $40
eggs truffles                                                        $35    $80

Ingredient Histogram: [0,0,0,0,1,0,1,1]
                       b p h m s o g t
 
  
  -}