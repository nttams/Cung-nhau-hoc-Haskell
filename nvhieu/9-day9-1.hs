{-
**************************** QUAN TRỌNG ********************* *******
Tuần này là một bài tập về nhà hai bước. Đầu tiên, bạn phải giải quyết
Thử thách "Mê cung" và sau đó là thử thách "Rừng". Những thách thức
nằm trong hai tệp riêng biệt trong cả bài tập về nhà và giải pháp, vì vậy
bạn có thể kiểm tra giải pháp cho thử thách "Mê cung" đầu tiên mà không cần
spoilers của "Rừng" một. Đảm bảo kiểm tra giải pháp cho
"Mê cung" (và chỉ "Mê cung," tôi thấy bạn 🥸👀) trước khi bắt đầu với
Thử thách "rừng"!
*************************************************** ******************
Hôm nay, bạn sẽ xây dựng trò chơi cơ bản và đơn giản nhất có thể tưởng tượng được.
Đây sẽ là một trò chơi mê cung trong đó người chơi phải viết một danh sách các bước di chuyển và trò chơi sẽ thực hiện chúng
và cho người chơi biết nó kết thúc ở đâu. Sau đó, người chơi có thể thay đổi nước đi và kiểm tra lại cho đến khi nó tìm thấy lối ra.
Để chơi trò chơi, người chơi sẽ mở GHCi, tải tệp này và chạy chức năng "solveMaze"
lấy một mê cung và một danh sách các bước di chuyển và trả về một Chuỗi có trạng thái kết quả.


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

Nó sẽ giống như thế này:
*Main> giải quyết Mê cung testMaze []
"Bạn vẫn đang ở trong mê cung. Hãy chọn một con đường, hỡi nhà thám hiểm dũng cảm: Đi sang trái, sang phải hoặc đi tiếp."
*Chính> giải quyết Mê cung kiểm tra Mê cung [GoLeft]
"Bạn đã đụng phải một bức tường!"
*Chính> giải quyết Mê cung kiểm tra Mê cung [GoForward]
"Bạn vẫn đang ở trong mê cung. Hãy chọn một con đường, hỡi nhà thám hiểm dũng cảm: Đi sang trái, sang phải hoặc đi tiếp."
*Main> giải quyết Mê cung kiểm tra Mê cung [GoForward, GoRight]
"Bạn đã đụng phải một bức tường!"
*Chính> giải quyết Mê cung kiểm tra Mê cung [Tiến lên, Đi sang trái]
"BẠN ĐÃ TÌM ĐƯỢC LỐI RA RỒI!!"



Làm thế nào bạn sẽ đạt được điều này? Bạn có thể thử nó một mình, nhưng ở đây bạn có một
từng bước chỉ trong trường hợp:
1. Viết hai kiểu dữ liệu. Một cho các nước đi (Move) mà bạn có thể thực hiện, và một cho mê cung (Maze).
(Sử dụng ví dụ trên để tìm ra chúng.)
2. Viết một hàm gọi là "di chuyển" nhận vào một mê cung và một lần di chuyển và trả về mê cung sau khi di chuyển.
3. Viết giá trị "testMaze" kiểu "Maze" và kiểm tra chức năng "di chuyển" trong GHCi.
4. Viết hàm "solveMaze" sẽ lấy một mê cung và danh sách các bước di chuyển rồi trả về mê cung
sau khi thực hiện những động thái đó.
5. Nếu bạn kiểm tra chức năng "solveMaze", bạn sẽ thấy rằng mỗi khi bạn cố gắng giải mê cung,
nó sẽ in toàn bộ mê cung cho người chơi xem. Để tránh điều đó, hãy viết hàm "showCurrentChoice"
lấy một mê cung và trả về một chuỗi khác tùy thuộc vào việc bạn va vào tường, tìm thấy lối ra hay
vẫn cần phải thực hiện một sự lựa chọn khác.
6. Điều chỉnh chức năng "solveMaze" thích ứng để sử dụng "showCurrentChoice" và chơi với trò chơi mới của bạn bằng GHCi! :D
-}

-- 1. Viết hai kiểu dữ liệu. Một cho các nước đi (Move) mà bạn có thể thực hiện, và một cho mê cung (Maze).
data Move = L | R | F  deriving (Show)
  
data Maze =  Exit | Wall |Inside Maze Maze Maze   deriving (Show)

-- 2. Viết một hàm gọi là "move" nhận vào một Maze và một Move và trả về Maze sau khi di chuyển.
move:: Maze -> Move -> Maze
move Wall _  = Wall
move Exit _  = Exit
move (Inside x _ _) L  = x
move (Inside _ x _) F = x
move (Inside _ _ x) R  = x


-- 3. Viết giá trị "testMaze" kiểu "Maze" và kiểm tra chức năng "move" trong GHCi.            
testMaze :: Maze
testMaze = Inside Wall (Inside Exit Wall Wall) (Inside Wall (Inside Wall Wall Wall) Exit)

-- 4. Viết hàm "solveMaze" sẽ lấy một mê cung và danh sách các bước di chuyển rồi trả về mê cung sau khi thực hiện những động thái đó.
{-4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.
5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.
-}
-- showCurrentChoice :: Maze -> Move -> String
showCurrentChoice :: Maze -> String
showCurrentChoice Wall = "You've hit a wall!"
showCurrentChoice Exit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice _ ="You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-- 6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D

solveMaze :: Maze -> [Move]  -> String
solveMaze maze moveList = showCurrentChoice $ foldl move maze moveList
