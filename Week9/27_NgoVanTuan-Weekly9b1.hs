
{- MAZE GAME

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
Hôm nay, bạn sẽ xây dựng trò chơi đơn giản và cơ bản nhất có thể tưởng tượng. Đó sẽ là một trò chơi mê cung, 
nơi người chơi phải nhập vào một list các bước đi, và trò chơi sẽ thực hiện chúng và thông báo cho người chơi 
nơi họ kết thúc. Sau đó, người chơi có thể thay đổi các bước đi và kiểm tra lại cho đến khi họ tìm thấy lối ra.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.
Để chơi trò chơi, người chơi sẽ mở GHCi, tải tệp này và chạy hàm "solveMaze" nhận vào một mê cung 
và một danh sách các bước đi và trả về một chuỗi với trạng thái kết quả.

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

HƯỚNG DẪN:

1. Viết hai kiểu dữ liệu. Một cho các bước đi (Move) bạn có thể thực hiện, và một cho mê cung (Maze).
(Dùng ví dụ trên để hình dung ra chúng.)

2. Viết một hàm "move" nhận vào một mê cung và một bước đi và trả về mê cung sau khi thực hiện bước đi đó.

3. Khai báo 1 giá trị "testMaze" có kiểu "Maze", và dùng nó để test hàm "move" trong GHCi.

4. Viết hàm "solveMaze" nhận vào một mê cung và một list các bước đi và trả về mê cung sau khi thực hiện các bước đi đó.

5. Nếu bạn kiểm tra hàm "solveMaze", bạn sẽ thấy rằng mỗi lần bạn cố gắng giải mê cung, nó sẽ in toàn bộ mê cung 
để người chơi thấy. Để tránh điều đó, viết một hàm "showCurrentChoice" nhận vào một mê cung và trả về một chuỗi khác nhau 
tùy thuộc vào việc bạn va vào tường, tìm thấy lối ra, hay vẫn cần phải chọn một đường khác.

6. Điều chỉnh hàm "solveMaze" để sử dụng "showCurrentChoice" và chơi với trò chơi mới của bạn bằng GHCi!
-}

data Move = GoLeft | GoForward | GoRight deriving (Show, Eq)
data Maze = Wall | Exit | Path Maze Maze Maze deriving (Show)

move :: Maze -> Move -> Maze
move Exit _ = Exit
move Wall _ = Wall
move (Path left forward right) m = case m of
    GoLeft -> left
    GoForward -> forward
    GoRight -> right

testMaze :: Maze
testMaze = Path Wall (Path Exit Exit Wall) Exit

showResult :: Maze -> String
showResult maze = case maze of
    Exit -> "YOU'VE FOUND THE EXIT!!"
    Wall -> "You've hit a wall!"
    _ -> "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveMaze :: Maze -> [Move] -> String
solveMaze maze moves = showResult result
    where
        result = foldl move maze moves

{- FOREST GAME

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

Chúng ta sẽ xây dựng dựa trên thử thách "Maze" bằng cách viết một trò chơi 
tương tự nhưng phức tạp hơn một chút.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

Trò chơi này hoạt động giống như trò chơi "Maze", với điểm khác biệt là 
người chơi hiện đang ở trong một khu rừng. Vì chúng ta đang ở trong rừng, 
nên không có tường. Và, nếu bạn đi đủ xa, bạn sẽ chắc chắn tìm thấy lối ra.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

Vậy, thách thức khi chơi trò chơi này là gì? 
Thách thức nằm ở chỗ giờ đây chúng ta có "sức bền". 
Sức bền là một con số (chúng ta bắt đầu với 10). Và, mỗi lần người chơi thực hiện một bước đi, 
sức bền của họ sẽ giảm xuống bằng lượng công việc cần thiết để vượt qua con đường hiện tại 
(được đại diện bởi một con số chứa trong giá trị constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Các kiểu dữ liệu và hàm về cơ bản là giống nhau, với một vài lưu ý:
- Chúng ta không có tường.
- Chúng ta không muốn chọn một kiểu số cụ thể, nhưng chúng ta muốn đảm bảo rằng 
có thể thực hiện các phép toán số học cơ bản với các giá trị số chúng ta truyền vào các hàm.
- Bởi vì bây giờ chúng ta phải theo dõi trạng thái sức bền của người chơi, chúng ta sẽ cần phải 
lưu trữ và cập nhật nó cùng với trạng thái hiện tại của khu rừng sau mỗi bước di chuyển. 
Đây sẽ là một trường hợp sử dụng tuyệt vời cho monads, nhưng vì chúng ta chưa biết cách 
dùng chúng, nên một cặp (stamina, forest) là những gì bạn cần.

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
-- Define the possible actions
data Action = GoLeft | GoRight | GoForward deriving (Show, Eq)

-- Define the Forest type (could be represented as a list of integers for simplicity)
type Forest = [Int]

-- Define the GameState which includes stamina and current location in the forest
type GameState = (Int, Forest)

solveForest :: Forest -> [Action] -> String
solveForest forest actions = evalGameState (10, forest) actions
  where
    evalGameState :: GameState -> [Action] -> String
    evalGameState (_, []) = "YOU'VE FOUND THE EXIT!!"
    evalGameState (stamina, _) (action:rest)
      | stamina <= 0 = "You ran out of stamina and died -.-!"
      | otherwise = case action of
          GoLeft -> evalGameState (stamina - 1, forest) rest
          GoRight -> evalGameState (stamina - 1, forest) rest
          GoForward -> evalGameState (stamina - 3, forest) rest

          -- If there are other actions to handle, add them similarly
    evalGameState (stamina, _) [] = 
      if stamina > 0
        then "You have " ++ show stamina ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
        else "You ran out of stamina and died -.-!"
