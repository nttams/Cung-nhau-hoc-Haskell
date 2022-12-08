-- Question 1
-- Write a multiline comment below.
{-Học Haskell
  Cùng nhau trao đổi.
 Tạo giá trị để xây dựng Cardano Blockchain -}
 
-- Question 2
-- Define a function that takes a value and multiplies it by 3.
multi x=x*3

-- Question 3
-- Define a function that calculates the area of a circle.

dientich r = pi * r * r

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
thetich r h = (dientich r)*h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
sosanh r h = (thetich r h) >= 42