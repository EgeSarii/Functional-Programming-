--Ege Sari s1034535
--Group 81

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
-}

intList :: Parser [Integer]
intList = (:) <$> symbol "{" *> many natural <* symbol "}"


{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Integer]
intRecord  = do { symbol "{"; n <-natural ; symbol "#"; record<-helper n ; symbol "}" ; return record }


helper :: Integer-> Parser [Integer]
helper 1 = (:[])<$> natural
helper n = (:) <$> natural <*> helper(n-1) 
