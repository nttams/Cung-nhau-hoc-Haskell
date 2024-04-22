-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?
-- Giả sử bạn đã định nghĩa các giá trị lồng nhau như bên dưới. Bạn sẽ làm thế nào để lấy giá trị của
-- 4 bằng cách chỉ sử dụng pattern matching trong một hàm?
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

get4Value :: [([Int], [Int])] -> Int
get4Value [(_, [_, x]), _] = x
get4Value rest = - 1

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
{- Viết một hàm nhận một danh sách các phần tử thuộc bất kỳ kiểu dữ liệu nào, 
     - Nếu danh sách có 3 phần tử trở lên, trả vể phần còn lại của danh sách với 3 phần tử đầu bị loại bỏ. 
     - Nếu không, không làm gì cả, trả về danh sách nguyên vẹn.
   Làm bằng hai cách, cách 1 với nhiều định nghĩa hàm và cách 2 với biểu thức case. -}
-- Pattern Matching
splitFirstByPatternMatching :: [Int] -> [Int]
splitFirstByPatternMatching [] = []
splitFirstByPatternMatching [x] = [x]
splitFirstByPatternMatching [x, y] = [x, y]
splitFirstByPatternMatching (x:rest) = rest

-- Case
splitFirstByCase :: [Int] -> [Int]
splitFirstByCase list = case list of
    [] -> []
    [x] -> [x]
    [x, y] -> [x, y]
    (x:rest) -> rest

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
-- Tạo một hàm nhận một bộ 3 phần tử (tất cả cùng kiểu Integer) và cộng chúng lại.
addThreeElements:: (Integer, Integer, Integer) -> Integer
addThreeElements (x, y, z) = x + y + z
   

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
-- Viết một hàm trả về True nếu nhận vào một danh sách rỗng và False nếu ngược lại.
checkEmpty :: [x]->Bool
checkEmpty list = case list of
    [] -> True
    _ -> False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
-- Viết lại hàm tail bằng cách sử dụng pattern matching. Nhưng, thay vì gặp lỗi nếu
-- danh sách là rỗng, thì trả về một danh sách rỗng cho trường hợp đó.
tail' :: [x]->[x]
tail' [] = []
tail' (_:rest) = rest

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
-- Viết hàm dùng biểu thức case: Nhận một số Int, và thêm 1 vào nó nếu nó là số chẵn. Ngược lại, không làm gì cả.
-- (Sử dụng hàm even để kiểm tra xem số đó có là số chẵn không.)
checkEven :: Int -> Int
checkEven n = case even n of
    True -> n+1
    _ -> n