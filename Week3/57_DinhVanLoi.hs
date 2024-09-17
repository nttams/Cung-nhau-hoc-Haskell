main :: IO ()
main = do
putStrLn "Main function"

-- GROUP3 checked, OK, but haskell seems can't print vietnamese (natively)
checkConsumption :: Double -> Double -> Double -> String 
checkConsumption p h maxAllowed
  | month > maxAllowed = "vượt quá lưu lượng tối đa cho phép"
  | month == maxAllowed = "bằng lưu lượng tối đa"
  | otherwise = "nhỏ hơn lưu lượng tối đa" 
  where
    month = p * h * 30

-- GROUP3 checked, NOT OK, don't know what is this
-- bai2
if (month > maxAllowed)
then [month - maxAllowed] "số điện năng tiêu thụ vượt quá giới hạn" 
else [maxAllowed - month] "số điện năng tiêu thụ tiết kiệm được"
-- bai3

-- GROUP3 checked, OK
calculateArea :: Double -> Double -> Double
calculateArea radius height = 
  let baseArea = pi * radius * radius
      cylinderVolume = baseArea * height
  in cylinderVolume
-- bai4

-- GROUP3 checked, NOT OK, does not compile
add2num :: Double -> Double -> Either String Double
add2num a b 
| a > b = if a == 0 
          then "error" 
          else if b == 0 
               then Left "error" 
               else Right (b / a)
| a < b = if a == 0
          then "error" 
          else if b == 0 
               then Left "error" 
               else Right (a / b)
| otherwise = " kết quả không thỏa mãn yêu cầu "