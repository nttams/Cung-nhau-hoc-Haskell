-- ------------------------
-- Bài tập 1 : Áp dụng Higher-order functions
-- ------------------------
{- 
Tìm số lớn thứ hai trong danh sách
Viết hàm secondLargest nhận vào một danh sách các số nguyên và trả về số lớn thứ hai trong danh sách đó.

-- Ví dụ:
-- ghci> secondLargest [1, 2, 3, 4, 5]
-- 4
-- ghci> secondLargest [10, 20, 30, 40, 50]
-- 40
-}
-- quick sort
descendingSort :: [Int] -> [Int]
descendingSort [] = []
descendingSort (x:xs) = 
    let smallerSort = descendingSort [a | a <- xs, a <= x]
        biggerSort = descendingSort [a | a <- xs, a > x]
    in  biggerSort ++ [x] ++ smallerSort

secondLargest :: [Int] -> Int
secondLargest a = (descendingSort a) !! 1

-- ------------------------
-- Bài tập 2 : Áp dụng Higher-order functions, Applying and composing functions 
-- ------------------------
{- 
Tính tổng các phần tử có chỉ số lẻ trong danh sách
Viết hàm sumOddIndices nhận vào một danh sách các số nguyên và trả về tổng các phần tử có chỉ số lẻ trong danh sách đó.
-- Ví dụ:
        [1, 2, 3, 4, 5, 6]
Chỉ số:  0  1  2  3  4  5

-- ghci> sumOddIndices [1, 2, 3, 4, 5, 6]
-- 12
        [10, 20, 30, 40, 50]
Chỉ số:   0  1   2   3   4 
-- ghci> sumOddIndices [10, 20, 30, 40, 50]
-- 60
-}
sumOddIndices :: [Int] -> Int
sumOddIndices a = sum $ getOddIndexValues (getOddIndex a)a

-- get a list of index
getOddIndex :: [Int] -> [Int]
getOddIndex x = [y | y <- [1..length x - 1], odd y]

--get a list of index value
getOddIndexValues :: [Int] -> [Int] -> [Int]
getOddIndexValues [] _ = []
getOddIndexValues (x:xs) a = (a !! x) : getOddIndexValues xs a

-- ------------------------
-- Bài tập 3 : Khuyến nghị dùng đệ quy
-- ------------------------
{- 
Viết hàm fibonacci nhận vào một số nguyên dương n và trả về số Fibonacci thứ n
Thứ tự      : 1 2 3 4 5 6 7  8  9  10
Fibonacci   : 0 1 2 3 5 8 13 21 34 55

-- Ví dụ:
-- ghci> fibonacci 5
-- 5
-- ghci> fibonacci 10
-- 55
-}
fibonacci :: Int -> Integer
fibonacci n = fibo !! n
            where fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

-- ------------------------
-- Bài tập 4 : Khuyến nghị dùng đệ quy
-- ------------------------
{- 
Tạo danh sách các số lẻ từ n đến m
Viết hàm oddNumbersInRange nhận vào hai số nguyên n và m, 
trả về danh sách các số lẻ từ n đến m.
-- Ví dụ:
-- ghci> oddNumbersInRange 1 10
-- [1, 3, 5, 7, 9]
-- ghci> oddNumbersInRange 10 20
-- [11, 13, 15, 17, 19]
-}
oddNumbersInRange :: Int -> Int -> [Int]
oddNumbersInRange n m = goTo [n..m]
    where
        goTo [] = []
        goTo (x:xs) 
            | odd x = x : goTo xs
            | otherwise = goTo xs

-- oddNumbersInRange :: Int -> Int -> [Int]
-- oddNumbersInRange n m = foldr (\a b -> if odd a then a : b else b) [] [n..m]

-- oddNumbersInRange' :: Int -> Int -> [Int]
-- oddNumbersInRange' n m = [x | x <- [n..m], odd x]

-- ------------------------
-- Bài tập 5 : Áp dụng Lambda, Foldl
-- ------------------------
{- 
Tính tổng các phần tử chẵn trong danh sách sử dụng foldl
Viết hàm sumEvenFoldl nhận vào một danh sách các số nguyên 
và trả về tổng các phần tử chẵn trong danh sách đó.

-- Ví dụ:
-- ghci> sumEvenFoldl [1, 2, 3, 4, 5]
-- 6
-- ghci> sumEvenFoldl [10, 21, 32, 43, 54]
-- 96
-}
sumEvenFoldl :: [Int] -> Int
sumEvenFoldl x = foldl (\b a -> if even a then a + b else b) 0 x

-- sumEvenFoldr :: [Int] -> Int
-- sumEvenFoldr x = foldr ((+) . \a -> if even a then a else 0) 0 x

-- sumEvenFoldl' :: [Int] -> Int
-- sumEvenFoldl' x = sum [a | a <- x, even a]

