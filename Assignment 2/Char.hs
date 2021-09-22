--Ege Sari s1034535
--Group 81

module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) a b = (map toLower a == map toLower b)

reverseCase :: String -> String
reverseCase x = map change x 
 where change z = if isUpper z then toLower z else toUpper z


shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n x = if x<='Z' && x>= 'A' then chr((mod(ord(x)+n-65) 26 )+ 65) else x -- The char character xs is shifted to right by the Int number n. 

caesar :: Int -> String -> String
caesar n xs = map (shift (n)) (map toUpper xs) --deciphers the ciphered text xs by shifting to left by the Int number n.

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"

--WE CAN DECODE msg WITH caesar 5 msg
--The secret message is : "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"