-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
-- Tìm hiểu về lớp kiểu `Bounded`. Nó cung cấp những behaviour nào?

Lớp kiểu Bounded trong Haskell được sử dụng cho các kiểu dữ liệu có giới hạn trên và giới hạn dưới. 
Nó cung cấp hai behavior chính:
minBound: Hàm này trả về giá trị nhỏ nhất của kiểu dữ liệu.
maxBound: Hàm này trả về giá trị lớn nhất của kiểu dữ liệu.

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{-
Các kiểu Int và Word thuộc cùng một lớp kiểu. Sự khác biệt giữa chúng là gì?
Hãy kiểm tra maxBound và minBound của chúng.
-}

Các kiểu Int và Word đều thuộc lớp kiểu Bounded, nhưng chúng khác nhau ở chỗ:
Int: Là kiểu số nguyên có dấu, nghĩa là nó có thể biểu diễn cả số âm và số dương.

minBound :: Int là -9223372036854775808
maxBound :: Int là 9223372036854775807
Word: Là kiểu số nguyên không dấu, nghĩa là nó chỉ biểu diễn các số không âm.

minBound :: Word là 0
maxBound :: Word là 18446744073709551615

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
-- Tìm hiểu về lớp kiểu `Enum`. Nó cung cấp những behaviour nào?

Lớp kiểu Enum trong Haskell được sử dụng cho các kiểu dữ liệu có thể liệt kê được. Nó cung cấp các behavior sau:

succ: Trả về giá trị kế tiếp của một giá trị cụ thể.
succ :: Enum a => a -> a
Ví dụ: succ 3 sẽ trả về 4.

pred: Trả về giá trị trước đó của một giá trị cụ thể.
pred :: Enum a => a -> a
Ví dụ: pred 3 sẽ trả về 2.

toEnum: Chuyển đổi từ một giá trị nguyên thành một giá trị của kiểu Enum.
toEnum :: Enum a => Int -> a
Ví dụ: toEnum 0 :: Bool sẽ trả về False.

fromEnum: Chuyển đổi từ một giá trị của kiểu Enum thành một giá trị nguyên.
fromEnum :: Enum a => a -> Int
Ví dụ: fromEnum False sẽ trả về 0.

enumFrom: Tạo một danh sách từ một giá trị bắt đầu.
enumFrom :: Enum a => a -> [a]
Ví dụ: enumFrom 3 sẽ trả về [3,4,5,...].

enumFromThen: Tạo một danh sách từ một giá trị bắt đầu và một giá trị bước.
enumFromThen :: Enum a => a -> a -> [a]
Ví dụ: enumFromThen 3 5 sẽ trả về [3,5,7,9,...].

enumFromTo: Tạo một danh sách từ một giá trị bắt đầu đến một giá trị kết thúc.

enumFromTo :: Enum a => a -> a -> [a]
Ví dụ: enumFromTo 3 7 sẽ trả về [3,4,5,6,7].

enumFromThenTo: Tạo một danh sách từ một giá trị bắt đầu, một giá trị bước, đến một giá trị kết thúc.
enumFromThenTo :: Enum a => a -> a -> a -> [a]
Ví dụ: enumFromThenTo 3 5 9 sẽ trả về [3,5,7,9].

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.
{-
Thêm chữ ký kiểu cho các hàm dưới đây và sử dụng biến kiểu và lớp kiểu.
Sau đó bỏ dấu chú thích ở trước các hàm và thử biên dịch.
-}

--f1 x y z = show (x / y) ++ z

--f2 x = if x == maxBound then minBound else succ x

f1 :: (Factorial a,Show a) => a -> a -> String -> String

f2 :: (Bounded a, Eq a, Enum a) => a -> a

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
{-
Tìm hiểu về các lớp kiểu số để tìm ra những behavior mà chúng cung cấp để chuyển đổi giữa các kiểu số.
-}

Các lớp kiểu số trong Haskell bao gồm Num, Real, Integral, Fractional, Floating, RealFrac, và RealFloat. 
Dưới đây là các behavior chính mà chúng cung cấp để chuyển đổi giữa các kiểu số khác nhau :

fromInteger: Chuyển đổi từ một giá trị Integer thành một giá trị của kiểu số.
fromInteger :: Num a => Integer -> a
Ví dụ:
x :: Int
x = fromInteger 42  -- x sẽ là 42

toRational: Chuyển đổi từ một giá trị của kiểu Real thành Rational.
toRational :: Real a => a -> Rational
Ví dụ:
r :: Rational
r = toRational (3.14 :: Double)  -- r sẽ là 157/50

toInteger: Chuyển đổi từ một giá trị của kiểu Integral thành Integer.
toInteger :: Integral a => a -> Integer
Ví dụ:
i :: Integer
i = toInteger (7 :: Int)  -- i sẽ là 7

fromRational: Chuyển đổi từ một giá trị Rational thành một giá trị của kiểu Fractional.
fromRational :: Fractional a => Rational -> a
Ví dụ:
f :: Double
f = fromRational (3 % 2)  -- f sẽ là 1.5

realToFrac: Chuyển đổi giữa các kiểu số thực (Real) và các kiểu số phân số (Fractional).
realToFrac :: (Real a, Fractional b) => a -> b
Ví dụ:
d :: Double
d = realToFrac (7 :: Int)  -- d sẽ là 7.0

pi: Giá trị của số pi.
pi :: Floating a => a
Ví dụ:
p :: Double
p = pi  -- p sẽ là 3.141592653589793

floatRadix: Cơ số của kiểu số dấu chấm động.
floatRadix :: RealFloat a => a -> Integer
Ví dụ:
radix :: Integer
radix = floatRadix (1.0 :: Float)  -- radix sẽ là 2

floatDigits: Số lượng chữ số trong cơ số của kiểu số dấu chấm động.
floatDigits :: RealFloat a => a -> Int
Ví dụ:
digits :: Int
digits = floatDigits (1.0 :: Double)  -- digits sẽ là 53

floatRange: Dải số mũ của kiểu số dấu chấm động.
floatRange :: RealFloat a => a -> (Int, Int)
Ví dụ:
range :: (Int, Int)
range = floatRange (1.0 :: Double)  -- range sẽ là (-1021, 1024)