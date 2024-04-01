-- Question 1
-- Multiline comment:
-- Hello.
-- I'm Minh 
-- From VN.

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
multiplyBy3 :: Num a => a -> a
multiplyBy3 x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
areaOfCircle :: Floating a => a -> a
areaOfCircle radius = pi * radius ^ 2

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder.
volumeOfCylinder :: Floating a => a -> a -> a
volumeOfCylinder radius height = areaOfCircle radius * height

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
checkVolume :: Floating a => a -> a -> Bool
checkVolume radius height = volumeOfCylinder radius height >= 42