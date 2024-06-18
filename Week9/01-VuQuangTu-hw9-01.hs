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


-- data Move = GoForward | GoLeft |GoRight
-- data Maza = Maza {
--     positon :: (Int,Int),
--     exit::(Int,Int),
--     walls ::[(Int,Int)]} deriving Show
    

-- move:: Maza->Move->Maza
-- move (Maza (x,y) exit walls ) move = Maza newPos exit walls
--     where
--         newPos =  case move of
--             GoForward -> (x,y+1)
--             GoLeft -> (x-1,y)
--             GoRight -> (x+1,y)


-- testMaze = Maza (0,0) (-1,1) [(1,0),(-1,0),(1,1)]


-- showCurrentChoice :: Maza->String
-- showCurrentChoice (Maza positon exit walls)
--     |positon == exit =  "YOU'VE FOUND THE EXIT!!"
--     |positon `elem` walls = "You've hit a wall!"
--     |otherwise= "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."


-- solveMaze :: Maza -> [Move]->String

-- solveMaze maza ms = showCurrentChoice (solveMazeToMaza maza ms )
--     where 
--         solveMazeToMaza :: Maza -> [Move]-> Maza
--         solveMazeToMaza maza [] = maza
--         solveMazeToMaza maza (m:ms) =solveMazeToMaza (move maza m) ms



-- Cach 2
data Move = GoLeft | GoForward |GoRight deriving Show
data Maze = Wall | Exit | Path Maze Maze Maze deriving Show

move :: Maze->Move->Maze
move Exit _ = Exit
move Wall _ =  Wall
move (Path left forward right) m = case m of
    GoLeft -> left
    GoForward ->forward
    GoRight -> right

testMaze :: Maze
testMaze = Path Wall (Path Exit Exit Wall) Exit 

showResult:: Maze->String

showResult maze = case maze of
    Exit -> "YOU'VE FOUND THE EXIT!!"
    Wall -> "You've hit a wall!"
    _ ->"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."


solveMaze :: Maze->[Move]->String
solveMaze maze moves = showResult result
    where
        result = foldl move maze moves





