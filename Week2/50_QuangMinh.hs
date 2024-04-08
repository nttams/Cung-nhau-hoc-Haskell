
-- Question 1: Define functions with correct type signatures
f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y / z)

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x / y - z)

f3 :: Bool -> a -> [Bool]
f3 x y = [x] ++ [True] 

f4 :: Eq a => a -> [a] -> [a] -> Bool
f4 x y z = x == head (y ++ z)  

-- Question 2: For error detection, documentation, code comprehension

-- Question 3: For clarity, readability, preventing errors 

-- Question 4: 
-- Type conversion
int2Double :: Int -> Double
int2Double x = fromIntegral x
-- Type transformation
x2List :: [Int] -> [Int]
x2List xs = map (\x -> x * 2) xs

-- Question 5
matrix :: [[Int]]
matrix = [[1,2,3],[4,5,6],[7,8,9]]
-- Index
ioe1 :: Int
ioe1 = matrix !! 1 !! 2  
-- Pattern matching
ioe2 :: Int -> [Int]
ioe2 n = matrix !! n 

--Main
main :: IO ()
main = do
    let r1 = f1 3.0 4.0 5.0
    let r2 = f2 6.0 7.0 8.0
    let r3 = f3 True "Hello"
    let r4 = f4 2 [2] [4]
    let r5 = int2Double 4
    let r6 = x2List [1,2,3,4,5]
    let r7 = ioe2 2

    putStrLn $ "Question 1, function 1: " ++ show r1
    putStrLn $ "Question 1, function 2: " ++ show r2
    putStrLn $ "Question 1, function 3: " ++ show r3
    putStrLn $ "Question 1, function 4: " ++ show r4
    putStrLn $ "Question 4, Type conversion: " ++ show r5
    putStrLn $ "Question 1, Type transformation: " ++ show r6
    putStrLn $ "Question 5, Pattern matching access " ++ show r7


    
