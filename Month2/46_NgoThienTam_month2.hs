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

-- assuming we are dealing with positive number only, so we can use negative number to indicate error
-- there's no second largest number in list that has less than 2 elements -> result = - 1
-- or the list has only 1 kind of value, it's the firstLargest -> no secondLargest -> result = -1
secondLargest :: [Int] -> Int
secondLargest l
    | length l < 2 = -1
    | otherwise = maxList $ filter (<maxList l) l
    where maxList = foldr (\x y -> if x > y then x else y) (-1)

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

extractIndex :: Bool -> [Int] -> [Int]
extractIndex _ [] = []
extractIndex condition (x:xs) = if condition then x : next else next
                                where next = extractIndex (not condition) xs

extractIndexEven = extractIndex True
extractIndexOdd = extractIndex False

sumOddIndices = sum . extractIndexOdd

-- ------------------------
-- Bài tập 3 : Khuyến nghị dùng đệ quy
-- ------------------------
{-
Viết hàm fibonacci nhận vào một số nguyên dương n và trả về số Fibonacci thứ n
Thứ tự      : 1 2 3 4 5 6 7  8  9  10
Fibonacci   : 0 1 2 3 5 8 13 21 34 55

Thứ tự      : 0 1 2 3 4 5 6 7  8  9  10
Fibonacci   : 0 1 1 2 3 5 8 13 21 34 55

-- Ví dụ:
-- ghci> fibonacci 5
-- 5
-- ghci> fibonacci 10
-- 55
-}

fibonacci :: Int -> Int
fibonacci n | n < 0 = -1 -- error case
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

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
oddNumbersInRange n m
    | n < 0 || m < 0 || n > m = []
    | otherwise = foldr (\x y -> if odd x then x:y else y) [] [n..m]

oddNumbersInRange2 :: Int -> Int -> [Int]
oddNumbersInRange2 n m = [x | x <- [n..m], odd x]

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
sumEvenFoldl = foldl (\x y -> if even y then x + y else x) 0