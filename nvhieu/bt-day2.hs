-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1::Float->Float->Float->Float
f1 x y z = x**(y/z)

f2::Float->Float->Float->Float
f2 x y z = sqrt (x/y - z)

f3::Bool->Bool->[Bool]
f3 x y = [x == True] ++ [y]

f4::[Char]->[Char]->[Char]->Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{-
     - Các biểu thức trong Haskell là bất biến. Chúng không thể thay đổi sau khi chúng được tạo.
     - Tính bất biến làm cho việc tái cấu trúc trở nên cực kỳ dễ dàng và viết mã dễ dàng hơn nhiều.
     - Để “thay đổi” một đối tượng, hầu hết các cấu trúc dữ liệu đều cung cấp các phương thức lấy đối tượng cũ và tạo một bản sao mới.
-}

-- Question 3
-- Why should you define type signatures for variables? How can they help you?

--Chúng giúp thể hiện cách dữ liệu thông qua một hàm, giúp bạn và những người khác nhanh chóng có được cái nhìn bao quát về hàm đó nên làm.

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

{-Để xác định đầu vào đã được cung cấp cụ thể cho ngữ cảnh này. Một số kiểu `Int` có thể đến từ bất kiểu gì, nhưng nếu bạn muốn khẳng định rằng nó đến từ một kiểu phù hợp với bài toán bằng cách sử dụng kiểu duy nhất, bạn có thể dựa vào điều đó
-}


-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

listOfNames :: [[[ Char ]]]
listOfNames = [[ " Hưng " , " Long " ], [ " Hải " , " Tiến " ], [ " Quang " , " Trung " ]]
{-
Đúng. Để truy cập danh sách ở giữa, ở trên, bạn sẽ sử dụng listOfNames !! 1

Bạn có thể lồng nhau hơn nữa (tức là danh sách của danh sách của danh sách) nhưng bạn phải sáng tạo về cách bạn nhắm mục tiêu dữ liệu bạn muốn truy xuất bên trong danh sách.
-}