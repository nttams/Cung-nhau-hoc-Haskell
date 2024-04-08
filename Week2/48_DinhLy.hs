
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)
-- Thêm chữ ký kiểu cho các hàm dưới đây, rồi bỏ comment và thử biên dịch (dùng các kiểu đã học trong bài)

-- f1 x y z = x ** (y/z)
f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

-- f2 x y z = sqrt (x/y - z)
f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

-- f3 x y = [x == True] ++ [y]
f3 :: Bool -> Bool -> Bool
f3 x y = [x == True] ++ [y]

-- f4 x y z = x == (y ++ z)
f4 :: Int -> Int -> Int -> Bool
f4 x y z = x == (y ++ z)

-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
-- Tại sao chúng ta nên định nghĩa chữ ký kiểu của các hàm? Chúng có thể giúp bạn như thế nào? 
-- Chúng có thể giúp người khác như thế nào?
{- Haskell là ngôn ngữ lập trình thuần hàm, khi định nghĩa chữ kí kiểu của các hàm, chúng ta có thể khai báo tên của hàm đó, loại kiểu của hàm đó sẽ hoạt động và 
bất biến, không thay đổi -}

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- Tại sao bạn nên định nghĩa chữ ký kiểu cho các biến? Chúng có thể giúp bạn như thế nào?
{- Việc định nghĩa kiểu dữ liệu cho biến sẽ giúp cho việc viết hàm rõ ràng hơn, ít gây ra sai sót do đã được xác định loại kiểu}

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- Trong Haskell, có những hàm nào cho phép bạn chuyển đổi từ kiểu này sang kiểu khác không?
-- Hãy thử tìm kiếm câu trả lời trên Google.
{- Hàm cho phép chuyển đổi từ kiểu này sang kiểu khác như là: Show, Read -}

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- Bạn có thể định nghĩa một danh sách của các danh sách trong Haskell không? 
-- Chúng ta đã gặp ví dụ nào về điều đó trong bài học chưa? 
-- Làm thế nào để bạn truy cập vào các phần tử bên trong cùng của nó?
list :: [[list]]
list = [[1..10], [12..88]]
{- list !! 0 -> [1..10] -}