-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
{-
Viết hàm `repeat'` nhận một giá trị và tạo ra một danh sách vô hạn với các phần tử là giá trị đó lặp đi lặp lại.
-}
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
{-
Sử dụng hàm `repeat'` và hàm `take` tạo một hàm gọi là `replicate'` nhận một số `n` và một giá trị `x`, rồi tạo ra một danh sách
có độ dài `n` với `x` là giá trị của mỗi phần tử. (`n` phải là số Integer.)
-}
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]
replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat' x)

-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
{-
Viết hàm `concat'` nối các list con trong một list of lists lại với nhau.
-}
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
{-
Viết hàm zip' nhận hai danh sách và trả về một danh sách các cặp tương ứng 
-}
-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
{-
Nếu một danh sách đầu vào ngắn hơn danh sách kia, các phần tử thừa của danh sách dài hơn
sẽ bị loại bỏ, kể cả khi một trong hai danh sách là vô hạn:
-}
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []
zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _           = []



-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
{-
Tạo một hàm `zipWith'` tổng quát hóa `zip'` bằng cách sử dụng một hàm (đối số đầu tiên) 
quy định cách ghép nối các phần tử, thay vì hàm tạo tuple.
-}
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
{-
Ví dụ, `zipWith' (+)` được áp dụng cho hai danh sách để tạo ra danh sách các tổng tương ứng:
-}
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
{-
Viết hàm `takeWhile'` nhận một hàm kiểm tra điều kiện và một danh sách.
Hàm trả về danh sách các phần tử thỏa mãn điều kiện cho đến khi gặp 1 phần tử không còn thỏa mãn.
-}
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []


-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.
{-
Viết một hàm nhận một số nguyên n, tính giai thừa n! 
trả về một chuỗi có dạng: "1*2* ... *n = n!" với n! là kết quả tính được.
-}
fact2String :: Integer -> String 
fact2String n =
    let 
        factorial 0 = 1
        factorial k = k * factorial (k - 1)
        fact = factorial n 
        factorialExp = show fact
    in 
        factorialExp
-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.
{-
Dưới đây bạn đã định nghĩa danh sách giá của 1 số loại bia trong `bevogBeerPrices` 
và danh sách đơn hàng của bạn trong `orderList`, cùng chi phí giao hàng `deliveryCost`. 
Viết một hàm nhận một đơn hàng và tính toán chi phí gồm cả chi phí giao hàng. 
Giả sử rằng hai danh sách có cùng thứ tự các loại bia.
-}

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Int)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

calculateTotalCost :: [(String, Int)] -> Double
calculateTotalCost order = sum [price * fromIntegral quantity | (beer, quantity) <- order, (b, price) <- bevogBeerPrices, beer == b] + deliveryCost




