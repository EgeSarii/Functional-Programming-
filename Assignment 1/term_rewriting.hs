{-

double 5 
=> {definition of double}
incr(incr 0)
=> {definition of incr}
incr(5+0)
=> {definition of +}
incr 5
=> {definition of incr}
5+5
=> {definition of +}
10



I have used applicative order for this rewriting.
-}
