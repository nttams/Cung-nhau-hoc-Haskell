
-- Question 1
-- Write a multiline comment below.
{- multiline comments
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
takeAndMultiple x = x * 3

-- Question 3
-- Define a function that calculates the area of a circle.
circleArea r = pi * r^2

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
cylinderVolume h r = h * circleArea r


-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.
checkVolume h r = 
    if cylinderVolume h r >= 42 
        then "the volume is greater than or equal to 42" 
        else "the volume is less than to 42"

