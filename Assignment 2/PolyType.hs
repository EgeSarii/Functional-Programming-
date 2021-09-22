--Ege Sari s1034535
--Group 81

module PolyType where

f8 :: Ord p => p -> p -> p
f8 x y  = if x <= y then x else y


f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

f10 :: (Eq a, Num a) => a -> a -> a
f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 :: p -> p -> p
f11 x y = get 0
  where get n = if n == 0 then x else y


  {-
  1.) 
    We can use String type on f8, because strings can be ordered such that for instance "e" <= "h" will be evaluated as TRUE.Also if
    we run the function f8 with the arguments "ege" "sari" we get "ege" because "ege" <= "sari" as string comparison.
    
    We can not use String type on f9, because it uses boolean types for making logical operations. But we can not use strings for these operations.
    
    We can not use String type on f10, because it uses numeral types like Integer or Double. But String type is not numeral .So for instance we 
    can not make an operation like this "ege" + "sari". So we can not use String type on f10.
  
    We can use String type on f11, because it just returns the first parameter and it could be a string also.
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
    f8 is a polymorphic function. It uses ad-hoc polymorphism.
    f9 is not a polymorphic function. Here 1 is Bool so the type of f9 is Bool -> Bool -> Bool
    f10 is a polymorphic function.It uses ad-hoc polymorphism.
    f11 is a polymorphic function. It uses parametric polymorphism.


  -}