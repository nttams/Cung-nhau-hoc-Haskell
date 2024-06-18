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

data Move = GoLeft | GoForward | GoRight
data Maze = FoundExit | HitWall | Door Maze Maze Maze deriving (Show)

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

move :: Maze -> Move -> Maze
move (Door m _ _) GoLeft = m
move (Door _ m _) GoForward = m
move (Door _ _ m) GoRight = m
move maze _ = maze

showCurrentChoice :: Maze -> String
showCurrentChoice HitWall = "You've hit a wall!"
showCurrentChoice FoundExit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice _ = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveMaze :: Maze -> [Move] -> String
solveMaze maze moves = showCurrentChoice $ foldl move maze moves

{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Forest a = FoundExitForest | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

testForest :: Forest Int
testForest =
    Trail 3
    (Trail 7
        (Trail 3 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 4 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 5 FoundExitForest FoundExitForest FoundExitForest)
    )
    (Trail 3
        (Trail 3 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 9 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 5 FoundExitForest FoundExitForest FoundExitForest)
    )
    (Trail 5
        (Trail 3 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 4 FoundExitForest FoundExitForest FoundExitForest)
        (Trail 1 FoundExitForest FoundExitForest FoundExitForest)
    )

moveForest :: Num a => (a, Forest a) -> Move -> (a, Forest a)
moveForest (staminaLeft, FoundExitForest) _ = (staminaLeft, FoundExitForest)
moveForest (staminaLeft, Trail stamina forest _ _) GoLeft = (staminaLeft - stamina, forest)
moveForest (staminaLeft, Trail stamina _ forest _) GoForward = (staminaLeft - stamina, forest)
moveForest (staminaLeft, Trail stamina _ _ forest) GoRight = (staminaLeft - stamina, forest)

showCurrentChoiceForest :: (Show a, Ord a, Num a) => (a, Forest a) -> String
showCurrentChoiceForest (s, _) | s <= 0 = "You ran out of stamina and died -.-!"
showCurrentChoiceForest (_, FoundExitForest) = "YOU'VE FOUND THE EXIT!!"
showCurrentChoiceForest (s, _) = "You have " ++ show s ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveForest :: (Show a, Ord a, Num a) => Forest a -> [Move] -> String
solveForest forest moves = showCurrentChoiceForest (foldl moveForest (10, forest) moves)
