import Data.List
-- Câu hỏi 1
-- Viết một hàm gọi là `repeat'` nhận một giá trị và tạo một danh sách vô hạn với giá trị được cung cấp dưới dạng mọi thành phần của danh sách.
--
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...

repeat' :: a -> [a]
repeat' a = a : repeat' a

-- Câu hỏi 2
-- Sử dụng hàm `repeat'` và hàm `take` mà chúng ta đã định nghĩa trong bài học (đi kèm với Haskell), tạo một hàm gọi là `replicate'` nhận vào một số `n` và một giá trị `x` rồi tạo một danh sách có độ dài `n` với `x` là giá trị của mọi phần tử. (`n` phải là Số nguyên.)
--
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]

replicate' :: Int -> a -> [a]
replicate' n x = take n $ repeat' x

-- Câu 3
-- Viết một hàm gọi là `concat'` nối một danh sách các danh sách.
--
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]

concat' :: [[a]] -> [a]
concat' ([]:(xxs)) = concat' xxs
concat' ((x:xs):(xxs)) = x : concat' ((xs):(xxs))
concat' _ = []


-- Câu 4
-- Viết một hàm gọi là `zip'` nhận hai danh sách và trả về một danh sách các cặp tương ứng (zip chúng) như sau:
--
-- zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- Nếu một danh sách đầu vào ngắn hơn danh sách kia, các phần tử thừa của danh sách dài hơn sẽ bị loại bỏ, ngay cả khi một trong các danh sách là vô hạn:
--
-- zip' [1] ['a', 'b']
-- [(1,'a')]
-- zip' [1, 2] ['a']
-- [(1,'a')]
-- zip' [] [1..]
-- []
-- zip' [1..] []
-- []

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys
--zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys

-- Câu 5
-- Tạo một hàm gọi là `zipWith'` khái quát hóa `zip'` bằng cách nén với một hàm được cung cấp làm đối số đầu tiên, thay vì một hàm tupling.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- Ví dụ: `zipWith' (+)` được áp dụng cho hai danh sách để tạo danh sách tổng tương ứng:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]


-- Câu 6
-- Viết một hàm gọi là `takeWhile'` nhận vào một tiền tố và một danh sách rồi trả về danh sách cho đến khi một phần tử không thỏa mãn vị từ.
--
-- takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- takeWhile (< 0) [1,2,3]
-- []


-- Câu 7 (Khó hơn)
-- Viết hàm nhận vào số nguyên n, tính giai thừa n! và trả về một chuỗi ở dạng 1*2* ... *n = n! ở đâu! là kết quả thực tế.
{-
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys
-}
-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]

zipWith' :: (a -> a -> a) -> [a] -> [a]-> [a]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile':: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = case (p $ head xs) of
  True -> head xs : (takeWhile' p $ tail xs)
  False -> []

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.

factorial :: Int -> String
factorial n
  | n < 0 = "Can't be a negative integer!"
  | n <= 1 = show n ++ "! -> 1 = 1"
  | otherwise = show n ++ "! -> " ++ (printFact n) ++ " = " ++ show (calcFact n)
      where calcFact 0 = 1
            calcFact n = n * calcFact (n-1)
            printFact n = concat (intersperse "*" (map show [1..n]))

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

calcTotalOrderCost :: [(String, Double)] -> Double
calcTotalOrderCost [] = 0
calcTotalOrderCost order =
  let beerPrices =  snd $ unzip bevogBeerPrices
      orderUnits = snd $ unzip order
      subtotals = zipWith' (*) orderUnits beerPrices
  in deliveryCost + (foldr (+) 0 subtotals)
