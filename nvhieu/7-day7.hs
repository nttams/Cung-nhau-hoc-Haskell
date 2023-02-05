-- Câu hỏi 1
-- Tiềm hiểu lớp kiểu `Bounded`. Những hành vi nó cung cấp?
{-
:i Bounded
type Bounded :: * -> Constraint
class Bounded a where
  minBound :: a
  maxBound :: a
-}

-- Câu hỏi 2
-- Các kiểu Int và Word thuộc các lớp cùng kiểu. sự khác biệt giữa chúng là gì? Có thể kiểm tra tham số maxBound và minBound cho cả hai loại.
{-
Kiểu Int là gồm các số nguyên cả âm và dương
Kiểu Word là kiểu của số nguyên dương.
Prelude> maxBound :: Int
9223372036854775807
Prelude> maxBound :: Word
18446744073709551615
Prelude> minBound :: Int
-9223372036854775808
Prelude> minBound :: Word
0
Prelude>
-}

-- Câu 3
-- Tìm hiểu lớp kiểu `Enum`.Những hành vi?
{-
Prelude> :i Enum
type Enum :: * -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
    -- Defined in ‘GHC.Enum’
instance Enum Word -- Defined in ‘GHC.Enum’
instance Enum Ordering -- Defined in ‘GHC.Enum’
instance Enum Integer -- Defined in ‘GHC.Enum’
instance Enum Int -- Defined in ‘GHC.Enum’
instance Enum Char -- Defined in ‘GHC.Enum’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Enum () -- Defined in ‘GHC.Enum’
instance Enum Float -- Defined in ‘GHC.Float’
instance Enum Double -- Defined in ‘GHC.Float’
-}

-- Câu 4
-- Thêm chữ ký kiểu vào các hàm bên dưới và sử dụng các biến kiểu và lớp kiểu. Sau đó bỏ ghi chú các chức năng và cố gắng biên dịch.
f1 :: (Fractional a, Show a)  => a -> a -> String -> String
f1 x y z = show (x / y) ++ z


f2 :: (Bounded b, Enum b, Eq b)=> b -> b
f2 x = if x == maxBound then minBound else succ x

-- Câu 5

-- Tìm hiểu các lớp kiểu số để tìm ra những hành vi mà chúng cung cấp để thay đổi giữa các kiểu số.

--Fractional 
    fromRational :: Rational -> a
--Real 
    toRational :: a -> Rational
--Integer
    fromInteger :: Integer -> a
--Integral
    toInteger :: a -> Integer
