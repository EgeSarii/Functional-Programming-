--Ege Sari s1034535
--Group 81

module Obfuscate where

import Data.Char
import Data.List


cambridge :: String -> String
cambridge xs = intercalate " " (map mix (words xs))


mix :: String -> String
mix [] = []
mix [a] = [a]
mix xs 
 | (sort (tail(init xs)) == (tail(init xs))) && length(lastLetter xs)==2 = head xs : reverse(sort (tail(init(init xs)))) ++lastLetter xs 
 | (sort (tail(init xs)) /= (tail(init xs))) && length(lastLetter xs)==2 = head xs : (sort (tail(init(init xs)))) ++lastLetter xs
 | (sort (tail(init xs)) == (tail(init xs))) && length(lastLetter xs)==1 = head xs : reverse(sort (tail(init xs))) ++lastLetter xs 
 | (sort (tail(init xs)) /= (tail(init xs))) && length(lastLetter xs)==1 = head xs : (sort (tail(init xs))) ++lastLetter xs

lastLetter :: String -> String
lastLetter xs = if ((last xs) <= 'z' && last xs >= 'A') then [last xs] else (last(init xs): last xs:[])       


meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."
