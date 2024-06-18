{-

**************************** IMPORTANT ****************************

This week is a two-step homework. First, you have to solve the
"Maze" challenge, and then the "Forest" challenge. The challenges
are in two separate files in both the homework and solution, so
you can check the solution for the first "Maze" challenge without
spoilers of the "Forest" one. Make sure to check the solution for
"Maze" (and only "Maze," I see you 🥸👀) before starting with the
"Forest" challenge!

*******************************************************************

Today, you'll build the simplest and most basic game imaginable.
It'll be a maze game where the player has to write a list of moves, and the game will perform them
and tell the player where it ends up. Then, the player can change the moves and check again until it
finds the exit.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.

It should look like this:

*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"

How are you going to achieve this? You can try it on your own, but here you have a
step-by-step just in case:

1. Write two data types. One for the moves (Move) you can make, and another for the maze (Maze).
(Use the example above to figure them out.)

2. Write a function called "move" that takes a maze and a move and returns the maze after the move.

3. Write a "testMaze" value of type "Maze" and test the "move" function in GHCi.

4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.

5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.

6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D
-}

-- 1.
data Move = GoLeft | GoForward | GoRight deriving (Show)

data Maze = FoundExit | HitWall | Door Maze Maze Maze deriving (Show)
-- 2.
move :: Maze -> Move -> Maze
-- move HitWall _ = HitWall
-- move FoundExit _ = FoundExit
move maze GoLeft = mapLeft maze
move maze GoRight = mapRight maze
move maze GoForward = mapMiddle maze

mapLeft:: Maze -> Maze
mapLeft FoundExit = FoundExit
mapLeft HitWall = HitWall
mapLeft (Door left middle right) = left

mapRight:: Maze -> Maze
mapRight FoundExit = FoundExit
mapRight HitWall = HitWall
mapRight (Door left middle right) = right

mapMiddle:: Maze -> Maze
mapMiddle FoundExit = FoundExit
mapMiddle HitWall = HitWall
mapMiddle (Door left middle right) = middle

-- 3.
testMaze :: Maze
testMaze = 
    Door
        HitWall
        (Door FoundExit HitWall HitWall)
        (Door
            HitWall
            (Door HitWall HitWall HitWall)
            HitWall
        )

-- 4.
solveMaze :: Maze -> [Move] -> Maze
solveMaze maze [] = maze
solveMaze maze (x:xs) = solveMaze (move maze x) xs

-- 5.
showCurrentChoice :: Maze -> String
showCurrentChoice FoundExit = "Congrats!, you found the exit"
showCurrentChoice HitWall = "Unfortunately, you hit a wall"
showCurrentChoice (Door _ _ _) = "you still stay in Maze :("

testFinalGame1 = showCurrentChoice $ solveMaze testMaze []
testFinalGame2 = showCurrentChoice $ solveMaze testMaze [GoLeft]
testFinalGame3 = showCurrentChoice $ solveMaze testMaze [GoForward]
testFinalGame4 = showCurrentChoice $ solveMaze testMaze [GoForward, GoRight]
testFinalGame5 = showCurrentChoice $ solveMaze testMaze [GoForward, GoLeft]