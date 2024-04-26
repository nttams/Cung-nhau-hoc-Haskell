-- Instructions:
-- Write a function that returns the earned score in three tosses of a Darts game.

-- Darts is a game where players throw darts at a target.	

-- In our particular instance of the game, the target rewards 4 different amounts of points, depending on where the dart lands:

-- Our dart scoreboard with values from a complete miss to a bullseye

-- If the dart lands outside the target, player earns no points (0 points).
-- If the dart lands in the outer circle of the target, player earns 1 point.
-- If the dart lands in the middle circle of the target, player earns 5 points.
-- If the dart lands in the inner circle of the target, player earns 10 points.
-- In case the player continuously throws the dart out of the board 2 times, he earns the point total is 0,
-- and if the player continuously throws the dart in the inner circle 2 times, he earns extra 10 points (this case he gets 30 points for 2 times).
-- and if the player continuously throws the dart in the inner circle 3 times, he earns total 50 points.

-- The outer circle has a radius of 10 units (this is equivalent to the total radius for the entire target), the middle circle a radius of 5 units, and the inner circle a radius of 1. Of course, they are all centered at the same point — that is, the circles are concentric defined by the coordinates (0, 0).

-- Write a function that takes in 3 points where darts are thrown on a target board (determined by Cartesian coordinates x and y, where x and y are real numbers), and returns the total score the player earned.

-- Reference source: https://exercism.org/tracks/haskell/exercises/darts

getPoint :: (Float, Float) -> Int
getPoint (x, y) 
      | z > 10 = 0
      | z <= 10 && z > 5 = 1
      | z <= 5 && z > 1 = 5
      | z <= 1 = 10
      where z = sqrt(x**2 + y**2)

totalScore :: Int -> Int -> Int -> Int
totalScore 0 0 _ = 0
totalScore _ 0 0 = 0
totalScore 10 10 10 = 50
totalScore 10 10 x = 30 + x
totalScore x 10 10 = x + 30
totalScore x y z = x + y + z

finalScore :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Int
finalScore a1 a2 a3 = totalScore (getPoint a1) (getPoint a2) (getPoint a3)