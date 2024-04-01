giveMe x = take x [1..] 
-- [1..] is an infinite list of natural numbers that starts at 1.

multipliesBy3 x = x * 3

calucateAreaByR r = r * r * pi

calucateCylinder h r = h * calucateAreaByR r

checkVolumnCylinder h r = if calucateCylinder h r >= 42 then "Greater or equal by 42" else "Less than 42"



main :: IO ()
main = do
    -- Question 1
    {-
        This is my answer
    -}
    -- Question 2
     print $ multipliesBy3 7
     
     -- Question 3
     print $ calucateAreaByR 15
     
     -- Question 4
     print $ calucateCylinder 2 15
     
     -- Question 5
     print $ checkVolumnCylinder 2 15