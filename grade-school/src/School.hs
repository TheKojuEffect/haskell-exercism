module School (School, add, empty, grade, sorted) where

import Data.List
import Data.Ord (comparing)

data School = School [(Int, String)]

add :: Int -> String -> School -> School
add gradeNum student (School students) = School ((gradeNum, student) : students)

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum (School students) = sort $ map snd $ filter (\student -> gradeNum == fst student) students

sorted :: School -> [(Int, [String])]
sorted (School students) = map gradeGrouper $ groupBy sameGrade $ sortBy gradeComparator students
                            where
                                gradeComparator = comparing fst
                                sameGrade s1 s2 = fst s1 == fst s2
                                gradeGrouper (student:students) = (fst student, sort $ (snd student) : map snd students)
