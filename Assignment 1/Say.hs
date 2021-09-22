module Say where

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteen"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"

say n = divideforThousand n
divideforThousand :: Integer -> String
divideforThousand n 
 |(div n 1000) /= 0 && rem n 1000 == 0 = divideby (div n 1000) ++ " thousand"
 |(div n 1000) /= 0 && rem n 1000 /= 0 =  divideby (div n 1000) ++ " thousand " ++ divideby (rem n 1000)
 |otherwise =  divideby (rem n 1000)
divideby :: Integer-> String
divideby n 
 | (div n 100) /= 0 && rem n 100 == 0 =say (quot n 100) ++ " hundred" 
 | (div n 100) /= 0 && rem n 100 /= 0 = say (quot n 100) ++ " hundred and "++ divideby10 (rem n 100)
 | otherwise = divideby10 (rem n 100)

divideby10  :: Integer -> String
divideby10 n
 | 20 <= n && (rem n 10 ==0) = say n
 | 20<= n && (rem n 10 /= 0) = say ((quot n 10)*10) ++ " " ++ say (rem n 10)
 |(div n 10 ) /=0 = say n
 |otherwise = say(rem n 10)
