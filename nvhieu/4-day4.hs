-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]->String
nested  [(_, [_,x]),(_,_)]=show x
nested _ ="Invalid input list"        

--nested = [([1,2],[3,4]), ([5,6],[7,8])]


-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
f2::[a]->[a]
f2 [_,_,_]=[]
f2 xs=xs
-------------
f2'::[a]->[a]
f2' x= case x of
  [_,_,_] -> []
  _ -> x

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
f3::(Int,Int,Int)->Int
f3 (x,y,z)=x+y+z

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
f4::[a]->Bool
f4 []=True
f4 _ =False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
f5::[a]->[a]
f5 [] = []
f5 (x:xs) =xs

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

f6 :: Int -> Int
f6 x = case even x of
    True    -> x+1
    False   -> x



--======================================================================================

whatsInsideThisList :: [Int] -> String
whatsInsideThisList [] = "It's empty!"
whatsInsideThisList [x] = "A single element: " ++ show x
whatsInsideThisList [x, y] = "Two elements: " ++ show x ++ " and " ++ show y
whatsInsideThisList (x:y:z:[]) = "The list has three elements: " ++ show [x,y,z]
whatsInsideThisList (x:xs)  = "The first element is: " ++ show xs ++ ", and there are quite a few more!"


initials' :: String -> String -> String  
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "." 
initials' (_) (_) = "How was your name again?"

checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes tuple3 = case tuple3 of
  (0, _, _) -> "The first one is a zero!"
  (_, 0, _) -> "The second one is a zero!"
  (_, _, 0) -> "The third one is a zero!"
  (_,_,_)   -> "We're good!"
  
greet :: String -> String
greet x
  | x=="Juan" = niceGreeting ++ " Juan!"
  | x== "Fernando" = niceGreeting ++ " Fernando!"
  | True = badGreeting ++ " " ++ x
  where niceGreeting = "Hello! So very nice to"
        badGreeting = "Oh! Pfft. It's you."

describeList :: [a] -> String
describeList ls = "The list is " ++ what ls 
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."