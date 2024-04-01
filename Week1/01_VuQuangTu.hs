
-- Question 1
-- Write a multiline comment below.
-- hear is comment1
-- hear is comment2
-- hear is comment3
-- hear is comment4


-- Question 2
-- Define a function that takes a value and multiplies it by 3.
func x=x*3

-- Question 3
-- Define a function that calculates the area of a circle.
circleArea radius = pi * radius * radius


-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
cylinderVolume radius height = circleArea radius* height

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
isVolumeCylinderGraterthan42 radius height = cylinderVolume radius height >=42

