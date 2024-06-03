-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
-- Tìm hiểu về lớp kiểu `Bounded`. Nó cung cấp những behaviour nào?

-- lớp kiểu Bounded là lớp kiểu được sử dụng để xác định giới hạn trên và dưới của một kiểu dữ liệu. 
-- Lớp này cung cấp hai hàm behaviour là minBound và maxBound, trích xuất giá trị nhỏ nhất và lớn nhất có thể của một kiểu dữ liệu cụ thể.
-- ví dụ:


-- minBound::Int = -9223372036854775808 (64bit)
--maxBound :: Int = 9223372036854775807 (64bit)

-- minBound::Bool = False
-- maxBound::Bool = True

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{-
Các kiểu Int và Word thuộc cùng một lớp kiểu. Sự khác biệt giữa chúng là gì?
Hãy kiểm tra maxBound và minBound của chúng.
-}

-- cả Int và Word là các kiểu số nguyên, nhưng có sự khác biệt về dấu và phạm vi giá trị có thể biểu diễn.

-- Int: Int là kiểu số nguyên có dấu, có thể biểu diễn số nguyên âm và dương, với phạm vi giới hạn bởi kiểu dữ liệu của hệ thống. 
-- phạm vi của nó có thể là từ khoảng -2^31 đến 2^31-1 hoặc -2^63 đến 2^63-1 tùy thuộc vào từng hệ thống

-- Word: Word là kiểu số nguyên không dấu, chỉ có thể biểu diễn số nguyên không âm, với phạm vi từ 0 đến 2^n-1, trong đó n tùy thuộc vào từng hệ thống


-- vi du : voi hệ thống 64 bit

-- maxBound:: Int = 2^63-1 = 9223372036854775807
-- minBound:: Int = -2^63 = -9223372036854775808


--maxBound :: Word = 2^64-1 = 18446744073709551615
--minBound :: Word = 0

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
-- Tìm hiểu về lớp kiểu `Enum`. Nó cung cấp những behaviour nào?



-- Lớp kiểu Enum được sử dụng để định nghĩa một tập hợp các kiểu dữ liệu có thể được liệt kê hoặc đếm.
--  Các kiểu dữ liệu trong lớp Enum có thể được chuyển đổi qua lại giữa dạng giá trị và dạng số nguyên, 
-- có thể liệt kê tuần tự, và hỗ trợ một số thao tác khác. 

-- các behavior chính mà lớp kiểu Enum cung cấp:


-- 1. Chuyển đổi giữa giá trị và số nguyên
-- toEnum :: Int -> a: Chuyển đổi từ một số nguyên sang giá trị thuộc kiểu a.
-- toEnum 0 :: Bool   -- Kết quả: False


-- fromEnum :: a -> Int: Chuyển đổi từ giá trị thuộc kiểu a sang số nguyên.
-- fromEnum True      -- Kết quả: 1



-- 2. Các hàm liệt kê tuần tự
-- succ :: a -> a: Lấy giá trị kế tiếp của một giá trị thuộc kiểu a.
-- succ False         -- Kết quả: True


-- pred :: a -> a: Lấy giá trị trước đó của một giá trị thuộc kiểu a.
-- pred True          -- Kết quả: False



-- 3. Tạo danh sách các giá trị
-- enumFrom :: a -> [a]: Tạo danh sách các giá trị bắt đầu từ giá trị cho trước đến giá trị tối đa của kiểu dữ liệu đó.
-- enumFrom False     -- Kết quả: [False,True]


-- enumFromThen :: a -> a -> [a]: Tạo danh sách các giá trị bắt đầu từ giá trị đầu tiên, bước nhảy là hiệu của hai giá trị đầu tiên.
-- enumFromThen False True  -- Kết quả: [False,True]


-- enumFromTo :: a -> a -> [a]: Tạo danh sách các giá trị từ giá trị bắt đầu đến giá trị kết thúc.
-- enumFromTo 3 7     -- Kết quả: [3,4,5,6,7]


-- enumFromThenTo :: a -> a -> a -> [a]: Tạo danh sách các giá trị bắt đầu từ giá trị đầu tiên, bước nhảy là hiệu của hai giá trị đầu tiên và dừng tại giá trị kết thúc.
-- enumFromThenTo 3 5 9 -- Kết quả: [3,5,7,9]


-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.
{-
Thêm chữ ký kiểu cho các hàm dưới đây và sử dụng biến kiểu và lớp kiểu.
Sau đó bỏ dấu chú thích ở trước các hàm và thử biên dịch.
-}

f1 :: (Fractional a,Show a)=>a->a->String->String
f1 x y z = show (x / y) ++ z

f2 :: (Bounded a, Eq a,Enum a)=>a->a
f2 x = if x == maxBound then minBound else succ x 


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
{-
Tìm hiểu về các lớp kiểu số để tìm ra những behavior mà chúng cung cấp để chuyển đổi giữa các kiểu số.
-}

-- 1) Num
-- kiểu cơ bản cho các kiểu số
-- behaviors: fromInteger :: Integer -> a 
--Chuyển đổi một giá trị kiểu Integer thành một kiểu số khác thuộc lớp Num.
--ví dụ: fromInteger 42 :: Double  -- Chuyển đổi từ Integer sang Double, kết quả: 42.0


--2) Real
--lớp kiểu sử dụng cho các số thực
-- behaviors: toRational :: a -> Rational
--Chuyển đổi một giá trị kiểu Real thành một kiểu số Rational
--ví dụ: toRational 42 ,kết quả: 42 % 1


--3)Integral:
--Lớp kiểu này dành cho các số nguyên.
-- behaviors : toInteger :: a -> Integer
-- Chuyển đổi một giá trị kiểu Integral (ví dụ Int hoặc Integer) thành Integer
--ví dụ: toInteger (42 :: Int) ,kết quả: 42.0


--4)Fractional:
--Lớp kiểu này dành cho các số có thể chia
-- behaviors : fromRational :: Rational -> a
-- Chuyển đổi một giá trị kiểu Rational thành một kiểu số khác thuộc lớp Fractional.
--ví dụ: fromRational (22 % 7) :: Float  -- Chuyển đổi từ Rational sang Float,kết quả: 3.142857



