-- Question 1
-- Write a multiline comment below.
{-
this is a multiple
line comment
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
multiplyBy3 :: (Num a) => a -> a
multiplyBy3 x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
calAreaCircle :: Float -> Float
calAreaCircle r = r * r * pi

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder.
calVolumeCylinder :: Float -> Float -> Float
calVolumeCylinder r h = h * (calAreaCircle r)

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
checkVolumeCylinder :: Float -> Float -> Bool
checkVolumeCylinder h r = (calVolumeCylinder r h) >= 42
