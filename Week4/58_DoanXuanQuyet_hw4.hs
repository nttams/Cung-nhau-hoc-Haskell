-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested  =  [([3,2],[43,4])]

getFour :: [([Int], [Int])] -> String
getFour ((_,[_,x]):_) = "the value of " ++ show x 

{-
Test_case 1 : getFour nested
 -> result : "the value of 4"
Test_case 2 : nested  = [([1,2],[3,4]), ([5,6],[7,8]), ([10,11],[12,13])]
-- "the value of 4"
Test_case 3 : nested  = [([3,2],[43,4])]
-- "the value of 4"
-}

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

checkListThreeElements :: [x] -> [x]
checkListThreeElements (x:y:z:rest) = rest
checkListThreeElements (x) = x

checkListThreeElementsCase :: [x] -> [x]
checkListThreeElementsCase checkList = case checkList of
    (x:y:z:rest) -> rest
    (x) -> x

{-
Test_case 1 : checkListThreeElements []
 -> result : []
Test_case 2 : checkListThreeElements [1]
-- result : [1]
Test_case 3 : checkListThreeElements [1,2,3]
-- result : []
Test_case 4 : checkListThreeElements [1,2,3,4]
-- result : [4]
Test_case 5 : checkListThreeElements [1,2,3,4,5,6,7,8]
-- result : [4,5,6,7,8]
Test_case 6 : checkListThreeElements [True,False,True,False,True,False]
-- result : [False,True,False]
Test_case 7 : checkListThreeElements "testcheckListThreeElements"
-- result : [tcheckListThreeElements]
-}

{-
Test_case 1 : checkListThreeElementsCase []
 -> result : []
Test_case 2 : checkListThreeElementsCase [1]
-- result : [1]
Test_case 3 : checkListThreeElementsCase [1,2,3]
-- result : []
Test_case 4 : checkListThreeElementsCase [1,2,3,4]
-- result : [4]
Test_case 5 : checkListThreeElementsCase [1,2,3,4,5,6,7,8]
-- result : [4,5,6,7,8]
Test_case 6 : checkListThreeElementsCase [True,False,True,False,True,False]
-- result : [False,True,False]
Test_case 7 : checkListThreeElementsCase "testcheckListThreeElements"
-- result : [tcheckListThreeElements]
-}

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

add3ElementTuple :: (Int,Int,Int) -> Int
add3ElementTuple (x,y,z) = x + y + z

{-
Test_case 1 : add3ElementTuple (2,3,4)
 -> result : 9
Test_case 2 : add3ElementTuple (9,0,1)
-- result : 10
Test_case 3 : add3ElementTuple (0,0,0)
-- result : 0
Test_case 4 : add3ElementTuple (19,110,1342)
-- result : 1471
-}

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.

checkEmptyList :: [x] -> Bool
checkEmptyList [] = True
checkEmptyList _ = False

{-
Test_case 1 : checkEmptyList []
 -> result : True
Test_case 2 : checkEmptyList [1]
-- result : False
Test_case 3 : checkEmptyList "test"
-- result : False
Test_case 4 : checkEmptyList "a"
-- result : False
Test_case 5 : checkEmptyList [True]
-- result : False
-}

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.

tail2 :: [x] -> [x]
tail2 [] = []
tail2 (x:rest) = rest

{-
Test_case 1 : tail2 []
 -> result : []
Test_case 2 : tail2 [1]
-- result : []
Test_case 3 : tail2 [1,2,3]
-- result : [2,3]
Test_case 4 : tail2 [1,2,3,4]
-- result : [2,3,4]
Test_case 5 : tail2 [1,2,3,4,5,6,7,8]
-- result : [2,3,4,5,6,7,8]
Test_case 6 : tail2 [True,False,True,False,True,False]
-- result : [False,True,False,True,False]
Test_case 7 : tail2 "testcheckListThreeElements"
-- result : [estcheckListThreeElements]
-}

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

evenAdd1 :: Int -> Int
evenAdd1 x = case even x of
    True -> x + 1
    False -> x

{-
Test_case 1 : evenAdd1 1
 -> result : 1
Test_case 2 : evenAdd1 2
-- result : 3
Test_case 3 : evenAdd1 0
-- result : 1
Test_case 4 : evenAdd1 (-1)
-- result : -1
Test_case 5 : evenAdd1 (-2)
-- result : -1
-}