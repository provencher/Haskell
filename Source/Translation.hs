{- # OPTIONS_GHC -XFlexibleInstances #-}

{-
In this program, i) hash tables may be used to look up words ("apple")
or $-padded prefixes ("app$$"); ii) trees may be used to look up words
("apple") or prefixes ("app"); and iii) lists may be used to look up
words ("apple") or fragments ("ppl").  To make the program slightly
more realistic, the tree instance must also be able to return multiple
definitions of the same word.

-}

module Main where

type Word       = String  -- no embedded blanks

type Definition = String  -- embedded blanks

-- a type class and three instances

class Dictionary dict where
  locate :: Word -> dict -> Definition

lookupIn :: Dictionary dict => dict -> Word -> Definition
lookupIn dictionary word = locate word dictionary

instance Dictionary Hash where
  locate word hashtable = retrieve word hashtable

instance Dictionary Tree where
  locate word tree = find word tree

instance Dictionary List where
  locate word list = search word list

-- English-French correspondances

raw1 =  [("the", "le"),("savage", "violent"),("work", "travail"),
         ("wild", "sauvage"),("chance", "occasion"),("than a", "qu'un"),
         ("expensive.", "cher."),("saves", "en vaut"),("time", "temps"),
         ("in", "<`a>"), ("worse", "pire"),("{", "{"),("A", "Un"),
         ("stitch", "point"),("crime;", "crime,"),("a", "une"),
         ("nine.", "cent."),("It's", "C'est"),("all", "tout"),
         ("rare", "rare"),("you", "vous"),("Abandon", "Abandonnez"),
         ("stash", "planquer"),("Everything", "Tout!ce!qui!est"),
         ("who enter.", "qui!entrez."),("Zazie", "Zazie"),("is", "est"),
         ("cat", "chat"),("it's", "c'est"),("raisin", "raisin sec"),
         ("hope,", "espoir,"),("mistake.", "faute."),("luck", "chance"),
         ("blueberry", "myrtille"),("I!took", "J'ai pris"),("that", "qui"),
         ("an", "un"),("enormous!risk.", "risque <e'>norme."),
         ("drink", "boisson"),("Live", "Vivre"),("regrets.", "regrets."),
         ("stone", "pierre"),("without", "sans"),("coat", "manteau"),
         ("broke", "a fait d<e'>border"),("The!straw", "La goutte d'eau"),
         ("camel's!back.", "vase."),("She", "Elle"),("He", "Il"),
         ("alcoholic!beverage", "boisson alcoolis<e'>e"),("for", "pour"),
         ("a!book", "un livre"),("that!class.", "cette mati<`e>re."),
         ("didn't!crack", "n'a m<e^>me pas ouvert"),("was", "a!<e'>t<e'>"),
         ("the enemy.", "l'ennemi."),("of", "de"),("in the", "<`a>!la"),
         ("pay", "solde1"),("balance", "solde2"),("sale", "solde3")]

data Entry = Entry {word       :: Word,
                    definition :: Definition}
             deriving (Eq, Show)

-- hash table: used for English-to-French translation

type Hash   = [Bucket]

type Bucket = [Entry]

-- Function to create a bucket for Tupples of Strings where the first element is of a given length
makeBucket :: [(String,String)] -> Int -> Bucket
makeBucket [] _ = []
makeBucket ((a,b):rest) c
 | length a == c = (Entry a b) : makeBucket rest c 
 | otherwise = makeBucket rest c



e2fDict =  [(makeBucket raw1 i) | i<- [1..maxL]]



maxL = maximum [length w | (w, d) <- raw1]

retrieve :: Word -> Hash -> Definition
retrieve w h = retrieve' w (h!!((length w)-1))


retrieve' :: Word -> Bucket -> Definition
retrieve' _ [] = "Not Found"
retrieve' "" _ = "Not Found"
retrieve' w ((Entry a b):rest)
	|w == a = b
	|otherwise = retrieve' w rest

-- binary-search tree: used for French-to-English translation

data Tree = Empty | Tree Entry (Tree) (Tree)

-- Converts the raw1 list to a list of entires, to a Tree of entries 
f2eDict = build (toEntry raw1)

toEntry :: [(String,String)] -> [Entry]
toEntry [] = []
toEntry ((a,b):rest) = (Entry b a) : (toEntry rest)

build :: [Entry] -> Tree
build []     = Empty
build (e:es) = ins e (build es)


-- place entry according to length of french word
ins :: Entry -> Tree -> Tree
ins (Entry [] []) (Tree a lst rst) = (Tree a lst rst)
ins e Empty = Tree e Empty Empty
ins e@(Entry w d) (Tree z@(Entry x y) lst rst)
    |   w <  x = Tree z (ins e lst) rst
    |   w >  x = Tree z lst (ins e rst)

find :: Word -> Tree -> String
find w = several . find' w

find' :: Word -> Tree -> [Definition]
find' _ Empty = []
find' w (Tree z@(Entry x y) lst rst)
    |   w == x = [y]
    |  	w .==. x = y:find' w lst ++ find' w rst 
    |   w < x = (find' w lst)
    |   w > x = (find' w rst)

several :: [Definition] -> String
several []  = "Not Found"
several [w] = w
several ws  = "{" ++ foldr1 bar ws ++ "}"
  where bar w s = w ++ " | " ++ s

-- Italian-French correspondances

raw2 = [("ogni", "tout"),("voi", "vous"),("Lasciate", "Abandonnez"),
        ("ch'entrate.", "qui!entrez."),("speranza,", "espoir,"),
        ("zzz", "zzz"),("ZZZ", "ZZZ"),("rimpianti.", "regrets."),
        ("Vivere", "Vivre"),("senza", "sans"),("create.", "cr<e'><e'>s."),
        ("non!fuor", "ne furent"),("cose", "choses"),("Dinanzi", "Avant"),
        ("a!me", "moi"),("cammin", "chemin"),("nostra", "notre"),
        ("Nel", "Au"),("del", "du"),("di", "de"),("vita.", "vie."),
        ("mezzo", "milieu")]

-- unsorted list: used for Italian-to-French translation

type List = [Entry]

i2fDict = toEntry2 raw2

toEntry2 :: [(String,String)] -> [Entry]
toEntry2 [] = []
toEntry2 ((a,b):rest) = (Entry a b) : (toEntry2 rest)

search :: Word -> List -> Definition
search w [] = "Not Found"
search w ((Entry a b):rest)
	| w == a = b
	| otherwise = search w rest

-- utilities for prefixes and fragments

(!==!) :: String -> String -> Bool  -- infix form of 'subseq'
[] !==!  _ = True
_  !==! [] = False
p  !==! t  = p .==. t || p !==! tail t

(.==.) :: String -> String -> Bool  -- infix form of 'prefix'
[] .==. _          = True
_  .==. []         = False
(x:xs) .==. (y:ys) = x == y && xs .==. ys

-- sentences to be translated

data Sentence = Sentence String Task

data Task = Eng2Fr | Fr2Eng | Ita2Eng | Ita2Fr
            deriving (Eq, Show)

eng1 = Sentence "A stitch in time saves nine." Eng2Fr

eng2 = Sentence "I!took an enormous!risk." Eng2Fr

eng3 = Sentence "The!straw that broke the camel's!back." Eng2Fr

eng4 = Sentence "Th$$$$$$$ th$$ br$$$ th$ ca$$$$$$$$$$$" Eng2Fr

eng5 = Sentence "She didn't!crack a!book for that!class." Eng2Fr

eng6 = Sentence "S$$ di$$$$$$$$$$ a!b$$$ f$$ th$$$$$$$$$" Eng2Fr

english = [eng1, eng2, eng3, eng4, eng5, eng6]

frn1 = Sentence "C'est pire qu'un crime, c'est une faute." Fr2Eng

frn2 = Sentence "Vivre sans regrets." Fr2Eng

frn3 = Sentence "V san re" Fr2Eng

frn4 = Sentence "Tout!ce!qui!est rare est cher." Fr2Eng

frn5 = Sentence "To rar est che" Fr2Eng

frn6 = Sentence "To ra es che" Fr2Eng

-- Tout ce qui est rare est cher.  Or un cheval bon march<e'> est rare,
-- donc un cheval bon march<e'> est cher.

frn7 = Sentence "Il a!<e'>t<e'> <`a>!la solde de l'ennemi." Fr2Eng

french = [frn1, frn2, frn3, frn4, frn5, frn6, frn7]

ita1 = Sentence "Lasciate ogni speranza, voi ch'entrate." Ita2Eng

ita2 = Sentence "Vivere senza rimpianti." Ita2Eng

italian1 = [ita1, ita2]

ita3 = Sentence "Dinanzi a!me non!fuor cose create." Ita2Fr

ita4 = Sentence "nan a!m on!fu co eate." Ita2Fr

ita5 = Sentence "Nel mezzo del cammin di nostra vita." Ita2Fr

italian2 = [ita3, ita4, ita5]

loop :: [Sentence] -> IO ()
loop [] = blankline
loop (x:xs) = do
  blankline
  process x
  loop xs

process :: Sentence -> IO ()
process thisSentence@(Sentence string _) = do
  render string
  render "==>"
  render $ translate thisSentence

translate :: Sentence -> String
translate (Sentence string task)

	| task == Eng2Fr	=  unwords (map (lookupIn e2fDict) (words string))

	| task == Ita2Eng	= translate (Sentence (unwords (map (lookupIn i2fDict) (words string))) Fr2Eng)

	| task == Fr2Eng	= unwords (use f2eDict (words string)) 

	| task == Ita2Fr	= unwords (use i2fDict (words string))   

  

use :: Dictionary dict => dict -> [Word] -> [String]
use dictionary wordlist = map (lookupIn dictionary) wordlist

-- print utilities

header :: String -> IO ()
header string = do
  blankline
  render string
  render $ replicate (length string) '_'

render = putStrLn

blankline = putStrLn ""

-- in case of fire, break glass

printBuckets :: [Bucket] -> IO ()
printBuckets = putStrLn . unlines. map showEntries

showEntries :: [Entry] -> String
showEntries = unlines . map show

printTree :: Tree -> IO ()
printTree = putStrLn . unlines . inorder

inorder :: Tree -> [String]
inorder Empty            = []
inorder (Tree e lst rst) = inorder lst ++ [show e] ++ inorder rst

main = do
  header "1) English-to-French translation"
  loop english
  header "2) French-to-English translation"
  loop french
  header "3) Italian-to-English translation"
  loop italian1
  header "4) Italian-to-French translation"
  loop italian2


{-


1) English-to-French translation
________________________________

A stitch in time saves nine.
==>
Un point <`a> temps en vaut cent.

I!took an enormous!risk.
==>
J'ai pris un risque <e'>norme.

The!straw that broke the camel's!back.
==>
La goutte d'eau qui a fait d<e'>border le vase.

Th$$$$$$$ th$$ br$$$ th$ ca$$$$$$$$$$$
==>
Not Found Not Found Not Found Not Found Not Found

She didn't!crack a!book for that!class.
==>
Elle n'a m<e^>me pas ouvert un livre pour cette mati<`e>re.

S$$ di$$$$$$$$$$ a!b$$$ f$$ th$$$$$$$$$
==>
Not Found Not Found Not Found Not Found Not Found


2) French-to-English translation
________________________________

C'est pire qu'un crime, c'est une faute.
==>
It's worse than a crime; it's a mistake.

Vivre sans regrets.
==>
Live without regrets.

V san re
==>
Live without regrets.

Tout!ce!qui!est rare est cher.
==>
Everything rare is expensive.

To rar est che
==>
Everything rare is expensive.

To ra es che
==>
Everything {raisin | rare} {hope, | is} expensive.

Il a!<e'>t<e'> <`a>!la solde de l'ennemi.
==>
He was in the {sale | balance | pay} of the enemy.


3) Italian-to-English translation
_________________________________

Lasciate ogni speranza, voi ch'entrate.
==>
Abandon all hope, you who enter.

Vivere senza rimpianti.
==>
Live without regrets.


4) Italian-to-French translation
________________________________

Dinanzi a!me non!fuor cose create.
==>
Avant moi ne furent choses cr<e'><e'>s.

nan a!m on!fu co eate.
==>
Not Found Not Found Not Found Not Found Not Found

Nel mezzo del cammin di nostra vita.
==>
Au milieu du chemin de notre vie.



-}