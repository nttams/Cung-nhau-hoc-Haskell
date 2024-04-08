
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)
-- Thêm chữ ký kiểu cho các hàm dưới đây, rồi bỏ comment và thử biên dịch (dùng các kiểu đã học trong bài)

f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
-- Tại sao chúng ta nên định nghĩa chữ ký kiểu của các hàm? Chúng có thể giúp bạn như thế nào?  
-- Chúng có thể giúp người khác như thế nào?
{-
  * Why should we define type signatures of functions?
   -> Answer:
   In Haskell, defining type signatures for functions is a fundamental aspect of the language and provides several benefits both for the developer and for others who might read or use the code
  * How can they help you?
   -> Answer:
    + Clarity and Understanding
    + Early Error Detection
    + Tooling and IDE Support
  * How can they help others?
   -> Answer:
    + Readability and Understandability
    + Ease of Integration and Use
    + Documentation and API Design
-}
-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- Tại sao bạn nên định nghĩa chữ ký kiểu cho các biến? Chúng có thể giúp bạn như thế nào?

{-
    * Why should define type signatures for variables?
    -> Answer:
    In Haskell, type signatures for variables are not commonly used in the same way as they are for functions. However, defining type signatures for variables can still be beneficial in certain situations
    * How can they help you?
    -> Answer:
    + Clarity and Documentation
    + Early Error Detection
    + Improved Readability
    + Facilitating Refactoring and Maintenance
-}

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- Trong Haskell, có những hàm nào cho phép bạn chuyển đổi từ kiểu này sang kiểu khác không?
-- Hãy thử tìm kiếm câu trả lời trên Google.
{-
    Answer:
    -> Yes
    Examples: toInteger, realToFrac,...
-}
-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- Bạn có thể định nghĩa một danh sách của các danh sách trong Haskell không? 
-- Chúng ta đã gặp ví dụ nào về điều đó trong bài học chưa? 
-- Làm thế nào để bạn truy cập vào các phần tử bên trong cùng của nó?

{-
    Can you also define in Haskell list of lists?
    -> Answer: listOfNumber = [0, 1, 2, ..]
    Did we showed any example of that?
    -> Answer: Yes
    How would you access the inner most elements?
    -> Answer: [1, 2, 3, 4, 5] !! 3
-}