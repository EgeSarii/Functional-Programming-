--Ege Sari s1034535
--Group 81

module Pow2 where

import GHC.Base

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)


--Int 63
--Integer 10000000      
--Integer 9000000
--Double 1023
--Float 127


{-
The maximum n for  Int is 63. Here is the explanation, If I type pow2 64 ::Int, then the result is 0. If I type
pow2 63 ::Int, then the result is -9223372036854775808. If I type maxInt, a function to determine the upper boundary of Int type,
I find the result is 9223372036854775807, the negative version of pow2 63. I have decided that n can be 63 maximum, because the
reason of negativity is the bit flip. Also we can see that in (pow2 63) -1  which is resulted as 9223372036854775807. So I will define
the max value for n is 63.



The maximum n for Integer is 9009000. Well actually I am not %100 sure but it creates exceptions after that point. In fact integer seems
it doesn't have boundaries but I am not so sure. I have run tests and I have found this number as n. 
The maximum n for Double is 1023. Thats the maximum n, after that poin for example 1024, it is valued as infinity.
The maximum n for Float is 127. Thats the maximum for n and the same as Double does, after 127, it is valued as infinity


-}