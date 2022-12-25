whatsInsideThisList :: [Int] -> String
whatsInsideThisList [] = "It's empty!"
whatsInsideThisList [x] = "A single element: " ++ show x
whatsInsideThisList [x, y] = "Two elements: " ++ show x ++ " and " ++ show y
whatsInsideThisList (x:y:z:[]) = "The list has three elements: " ++ show [x,y,z]
whatsInsideThisList (x:rest)  = "The first element is: " ++ show x ++ ", and there are quite a few more!"


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