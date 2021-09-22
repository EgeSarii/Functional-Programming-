module Database where

type Person = (Name, Age, FavouriteCourse)
type Students = [Person]

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

ege, elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
ege =     ("Ege", 21, "Software Verification")

students :: [Person]
students = [elena, peter, pol, ege]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (x, _, _) = x

favoriteCourse :: Person -> FavouriteCourse
favoriteCourse ( _ , _ , f) = f

--returns a string representation of a person
showPerson:: Person -> String
showPerson p = show (name p) ++ " is " ++ show (age p) ++ " years old and his/her favorite course is " ++ show p

--check if two person has the same age
twins :: Person -> Person -> Bool
twins p1 p2 
 |p1 == p2 = True
 |otherwise = False

--increase the age of person
increaseAge :: Person -> Person
increaseAge p = (name p,( age p )+1, favoriteCourse p)

--increment age of all students by two
increaseAge2 :: Students
increaseAge2 = map increAge students where increAge p =  ((name p), (age p +2), (favoriteCourse p))

--promotion with "dr"
promote :: Students
promote = let proDr p = (("dr. "++" " ++name p), (age p), (favoriteCourse p)) in map proDr students

--find students with name Frits
findFrits :: Students
findFrits = let isFrits p = name p == "Frits" in filter isFrits students

--find the people in twenties
findTwenties :: Students
findTwenties = let isInTwenties p = (age p<30 && age p >=20) in filter isInTwenties students

--average age of all students
findAverage :: Integer
findAverage = div (fromInteger(sum (map age students))) (toInteger(length students))

promoteFP :: Students
promoteFP = let proDr p = (("dr. "++" " ++name p), (age p), (favoriteCourse p)) in map proDr (filter favoriteFP students) where
    favoriteFP p = if favoriteCourse p == "Functional Programming" then True else False 

