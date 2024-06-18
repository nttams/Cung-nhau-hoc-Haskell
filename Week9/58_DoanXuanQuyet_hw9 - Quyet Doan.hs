-- **************************** MAZE GAME ****************************

data Maze = FoundExit | HitWall | Door Maze Maze Maze deriving (Show)
data Move = GoLeft | GoForward | GoRight deriving (Show)

instance Eq Maze where
    FoundExit == FoundExit = True
    HitWall == HitWall = True
    (Door l1 f1 r1) == (Door l2 f2 r2) = l1 == l2 && f1 == f2 && r1 == r2
    _ == _ = False

move :: Maze -> Move -> Maze
move (Door left _ _) GoLeft = left
move (Door _ forward _) GoForward = forward
move (Door _ _ right) GoRight = right

testMaze :: Maze
testMaze =  
    Door 
    HitWall
    (Door FoundExit HitWall HitWall)
    (   Door
        HitWall
        (Door HitWall HitWall HitWall)
        HitWall
    )

showCurrentChoice :: Maze -> String
showCurrentChoice HitWall = "You've hit a wall!"
showCurrentChoice FoundExit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice _ = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveMaze :: Maze -> [Move] -> String
solveMaze maze [] = showCurrentChoice maze
solveMaze maze (x:xs) = 
    let newMaze = move maze x
    in if newMaze == FoundExit || newMaze == HitWall
       then showCurrentChoice newMaze
       else solveMaze newMaze xs

{-
solveMaze testMaze []
 -> "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
solveMaze testMaze [GoLeft]
 -> "You've hit a wall!"
solveMaze testMaze [GoForward]
 -> "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
solveMaze testMaze [GoForward, GoRight, GoRight]
 -> "You've hit a wall!"
solveMaze testMaze [GoForward, GoLeft] 
solveMaze testMaze [GoForward, GoLeft, GoRight] 
 -> "YOU'VE FOUND THE EXIT!!"
-}

-- **************************** FOREST GAME ****************************

type Stamina = Int
data Forest a = YouFoundExit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show)

instance Eq a => Eq (Forest a) where
    YouFoundExit == YouFoundExit = True
    (Trail x1 l1 f1 r1) == (Trail x2 l2 f2 r2) = x1 == x2 && l1 == l2 && f1 == f2 && r1 == r2
    _ == _ = False

testForest :: Forest Stamina
testForest =
    Trail 3
    (Trail 7
        (Trail 3 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 4 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 5 YouFoundExit YouFoundExit YouFoundExit)
    )
    (Trail 3
        (Trail 3 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 9 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 5 YouFoundExit YouFoundExit YouFoundExit)
    )
    (Trail 5
        (Trail 3 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 4 YouFoundExit YouFoundExit YouFoundExit)
        (Trail 1 YouFoundExit YouFoundExit YouFoundExit)   
    )

run :: Stamina -> Forest Stamina -> Move -> (Stamina ,Forest Stamina)
run stamina YouFoundExit _ = (stamina, YouFoundExit)
run stamina (Trail a left _ _) GoLeft = (stamina - a,left)
run stamina (Trail a _ forward _) GoForward = (stamina - a,forward)
run stamina (Trail a _ _ right) GoRight = (stamina - a,right)

showResult ::  Forest Stamina -> Stamina -> String
showResult forest stamina 
    | stamina < 0 = "You ran out of stamina and died -.-!"
    | forest == YouFoundExit = "YOU'VE FOUND THE EXIT!!"
    | otherwise = "You have " ++ show stamina ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveForest  :: Forest Stamina -> [Move] -> String
solveForest forest moves =
    let 
        yourStamina = 10
        process (sta, for) mov = 
            if sta <= 0 || for == YouFoundExit then (sta, for)
            else run sta for mov
        (finalStamina, finalForest) = foldl process (yourStamina, forest) moves
    in showResult finalForest finalStamina

{-
solveForest testForest []
    -> "You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
solveForest testForest [GoForward ]
    -> "You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
solveForest testForest [GoForward, GoForward]
    -> "You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
solveForest testForest [GoForward, GoForward, GoLeft]
solveForest testForest [GoForward, GoForward, GoLeft , GoRight, GoRight, GoRight ]
    -> "You ran out of stamina and died -.-!"
solveForest testForest [GoForward, GoLeft , GoRight, GoRight, GoRight]
solveForest testForest [GoForward, GoLeft , GoRight, GoRight, GoRight]
solveForest testForest [GoForward, GoLeft , GoRight, GoRight, GoRight, GoRight, GoRight]
    -> "YOU'VE FOUND THE EXIT!!"
-}