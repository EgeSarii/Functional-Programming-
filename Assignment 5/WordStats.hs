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

anagrams :: String -> [[String]]
anagrams


main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)
