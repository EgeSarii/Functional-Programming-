module Obfuscate where

import Data.Char
import Data.List

cambridge :: String -> String
cambridge xs = intercalate " " (map mix (words xs))


mix :: (Ord a, Num a) => [a] -> [a]
mix [] = []
mix [a] = [a]
mix xs = if sort (tail(init xs)) == (tail(init xs)) then head xs : reverse(sort (tail(init xs))) ++[lastLetter xs] else 
       head xs : (sort (tail(init xs))) ++[last xs]

lastLetter :: (Ord a, Num a) =>[a]-> a
lastLetter xs = if ((last xs) < 123 && last xs > 64) then last xs else last(init xs)       


meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."