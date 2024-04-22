-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?
-- Giả sử bạn đã định nghĩa các giá trị lồng nhau như bên dưới. Bạn sẽ làm thế nào để lấy giá trị của
-- 4 bằng cách chỉ sử dụng pattern matching trong một hàm?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

--HD:

get4 :: [([Int],[Int])]->Int
get4 [(_, [_, a]), (_, _)] = a 
{-Bỏ qua phần tử đầu tiên của danh sách 1 đến phần tử thứ 2 là 1 list gồm 2 phần tử trong list đó
bỏ qua phần tử thứ nhất lấy phần tử thứ 2 là a(4) và tiếp theo bỏ qua danh sách thứ 2 -}


-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
{- Viết một hàm nhận một danh sách các phần tử thuộc bất kỳ kiểu dữ liệu nào, 
     - Nếu danh sách có 3 phần tử trở lên, trả vể phần còn lại của danh sách với 3 phần tử đầu bị loại bỏ. 
     - Nếu không, không làm gì cả, trả về danh sách nguyên vẹn.
   Làm bằng hai cách, cách 1 với nhiều định nghĩa hàm và cách 2 với biểu thức case. -}

--HD C1:

check3 :: [a]->[a] --Định nghĩa hàm có đầu vào 1 list bất kì
--Định nghĩa nhiều hàm 
check3 [] = []  
check3 [x] = [x] 
check3 [x,y] = [x,y] 
check3 (x:y:z:[]) = [x,y,z]
--Nếu có nhiều hơn 3 phần tử thì không quan tâm 3 phần tử đầu in ra phần sau của danh sách 
check3 (_:_:_:rest) = rest

--HD C2:

check3' :: [x] -> [x]
check3' x = case x of
    []          -> []
    [x]         -> [x]
    [x,y]      -> [x,y]
    (x:y:z:[]) -> [x,y,z]
    (_:_:_:rest) -> rest


-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
-- Tạo một hàm nhận một bộ 3 phần tử (tất cả cùng kiểu Integer) và cộng chúng lại.

--HD:

sum3INT :: (Int,Int,Int)->Int
sum3INT (a,b,c) = a+b+c

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
-- Viết một hàm trả về True nếu nhận vào một danh sách rỗng và False nếu ngược lại.

--HD:

checkEmpty :: [x]->Bool
checkEmpty []=True --TH list rỗng
checkEmpty _  = False --TH còn lại

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
-- Viết lại hàm tail bằng cách sử dụng pattern matching. Nhưng, thay vì gặp lỗi nếu
-- danh sách là rỗng, thì trả về một danh sách rỗng cho trường hợp đó.

--HD:
--Cần biết định nghĩa hàm tail: hàm tail sẽ trả về danh sách mà loại bỏ phần tử đầu tiên 
tail' :: [x]->[x]
tail' []=[] -- Nếu rỗng thì trả về rỗng 
tail' (_:rest)=rest --Không thì trả về list bỏ đi phần tử đầu 

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
-- Viết hàm dùng biểu thức case: Nhận một số Int, và thêm 1 vào nó nếu nó là số chẵn. Ngược lại, không làm gì cả.
-- (Sử dụng hàm even để kiểm tra xem số đó có là số chẵn không.)

--HD: 

checkEven :: Int -> Int
checkEven n = case even n of
    True -> n+1
    _ -> n -- Hoặc dùng False -> n
factorial :: Int -> Int
factorial n
    | n < 0 = error "Không thể tính giai thừa của số âm"
    | n == 0 = 1
    | otherwise = n * factorial (n - 1)
