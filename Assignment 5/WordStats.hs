--Ege Sari s1034535
--Group 81

import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

mostFrequentOfLength :: Int -> String -> [(String,Int)]
mostFrequentOfLength n = reverse.(filter (\(x,y)-> y>=n)). wordFrequency 

wordLengthFrequency :: String -> [(Int,Int)]
wordLengthFrequency  = sortOn fst . map (\(x,y) -> ((length x), y))  . wordFrequency

--I could have only grouped the words with the same length but couldn't implement a 
--method if they are anagram or not. If you can indicate how to do it in the feedback part, it would 
--be great for me. Thank you :)
anagrams :: String -> [[String]]
anagrams = groupByLength . sortOn fst . map (\(x,y) -> (length x, x))  . wordFrequency

groupByLength :: [(Int, String)] -> [[String]]
groupByLength [] = []
groupByLength [x] = [[snd x]]
groupByLength (x:y:xs) = if fst x == fst y then 
 (if length xs ==1 
   then (if fst(head xs) == fst y 
    then [snd x: snd y: snd(head xs):[]] else [(snd x: snd y:[])] ++ [[snd(head xs)]] )else  [snd x: snd y:[]] ++ groupByLength xs ) else [[snd x]] ++ groupByLength (y:xs) 
   

main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)

