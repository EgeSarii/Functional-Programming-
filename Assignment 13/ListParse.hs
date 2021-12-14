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
--(<*>) :: f(a->b) -> fa -> fb
--fmap:: (a->b) -> fa -> fb
parseAll :: Parser a -> String -> Maybe a
parseAll p inp = case parse p inp of
                   Just (x,[]) -> Just x
                   _          -> Nothing

intList :: Parser [Integer]
intList = (pure (++)) <*> P(\s->  case s!!0 of
                            '{' -> Just ([],  tail)
                             '1' -> Just (1, tail)
                             _ -> Nothing )

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

--intRecord :: Parser [Integer]
