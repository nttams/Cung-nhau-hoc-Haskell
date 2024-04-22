-- Q1: extract 4 from nested list
getVal :: [([Int], [Int])] -> Int
getVal [] = error "Empty"
getVal ((_, second):_) = 
    case second of
        [_, value] -> value
        _ -> error "No 4"

-- Q2: remove if a list has 3 elements else remain
-- multiple function definition
delLong :: [a] -> [a]
delLong xs
    | length xs >= 3 = []
    | otherwise = xs
-- case experssion
delLong' :: [a] -> [a]
delLong' xs = 
    case length xs >= 3 of
        True -> []
        False -> xs

-- Q3: Function to sum the elements of a 3-element tuple of Integers
sumTup :: (Integer, Integer, Integer) -> Integer
sumTup (x, y, z) = x + y + z

-- Q4: check if a list is empty
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- Q5 tail func ptmatching if list empty
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

-- Q6 case expression , +1 for even integer
addEven:: Int -> Int
addEven n =
    case even n of
        True  -> n + 1
        False -> n

-- MAIN
main :: IO ()
main = do 
    let nested = [([1,2],[3,4]), ([5,6],[7,8])]
    let l1 = [2,3,5,6]
    let l2 = [7,8]
    let emptyList = []
    let tuple = (1, 2, 3)
    let n1 = 2468
    let n2 = 3571

    let q2l1 = delLong (delLong l1)
    let q2l2 = delLong (delLong l2)
    let q2l1s = delLong' (delLong' l1)
    let q2l2s = delLong' (delLong' l2)
    let stuple = sumTup tuple
    let empty = isEmpty emptyList
    let nonEmpty = isEmpty l1
    let tailList = safeTail l2 
    let tailEmpty = safeTail emptyList
    let even = addEven n1
    let noneven = addEven n2 

    putStrLn $ "Q1 result: " ++ show nested
    putStrLn $ "Q2 mfd list 1 result: " ++ show q2l1
    putStrLn $ "Q2 mfd list 2 result: " ++ show q2l2
    putStrLn $ "Q2 ce list 1 result: " ++ show q2l1
    putStrLn $ "Q2 ce list 2 result: " ++ show q2l2
    putStrLn $ "Q3 tuple result: " ++ show stuple
    putStrLn $ "Q4 the list is empty: " ++ show empty
    putStrLn $ "Q4 the list is empty: " ++ show nonEmpty
    putStrLn $ "Q5 list result: " ++ show tailList
    putStrLn $ "Q5 list result: " ++  show tailEmpty
    putStrLn $ "Q6 result for 2468: " ++ show even
    putStrLn $ "Q6 result for 3571: " ++ show noneven


