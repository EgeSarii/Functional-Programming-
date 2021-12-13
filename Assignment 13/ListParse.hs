{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}
--newtype Parser a = P { parse :: String -> Maybe (a, String) }
--parse :: Parser a -> (String -> Maybe (a, String))
--intList :: Parser [Integer]
--intList = 

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

--intRecord :: Parser [Integer]
