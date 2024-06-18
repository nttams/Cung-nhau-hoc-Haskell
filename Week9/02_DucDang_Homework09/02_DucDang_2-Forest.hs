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

type Stamina = Int
data Move = GoLeft | GoForward | GoRight deriving (Show)
data Forest a = FoundExit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

move :: Forest Stamina -> Move -> Forest Stamina
move forest GoLeft = mapLeft forest
move forest GoRight = mapRight forest
move forest GoForward = mapMiddle forest

mapLeft:: Forest Stamina -> Forest Stamina
mapLeft FoundExit = FoundExit
mapLeft (Trail a left middle right) = left

mapRight:: Forest Stamina -> Forest Stamina
mapRight FoundExit = FoundExit
mapRight (Trail a left middle right) = right

mapMiddle:: Forest Stamina -> Forest Stamina
mapMiddle FoundExit = FoundExit
mapMiddle (Trail a left middle right) = middle

testForest :: Forest Stamina
testForest =
    Trail
     3
     ( Trail
        7
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
     )
     ( Trail
        3
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 9 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
     )
     ( Trail
        5
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 1 FoundExit FoundExit FoundExit)
     )

solveForest :: Forest Stamina -> [Move] -> Forest Stamina
solveForest forest [] = forest
solveForest forest (x:xs) = solveForest (move forest x) xs

stamina :: Forest Stamina -> Stamina
stamina (Trail a left middle right) = a

-- stamina after each steps
-- staminaAfterSteps :: Forest Stamina -> [Move]-> [Stamina]
-- staminaAfterSteps forest [] = [10]
-- staminaAfterSteps forest mList= foldr (\m b-> b + (stamina $ solveForest forest [m])) 0 mList

-- Note: for the function "showCurrentChoice", I cannot process it yet for now. Sorry for that.
-- showCurrentChoice :: Forest Stamina -> String