{-
**************************** QUAN TRỌNG ********************* *******
Giải bài tập về nhà này sau khi hoàn thành và kiểm tra "Mê cung" một.
*************************************************** ******************
Chúng ta sẽ xây dựng thử thách "Mê cung" bằng cách viết mã một trò chơi tương tự nhưng phức tạp hơn một chút.
Nó hoạt động giống như trò chơi "Mê cung", với sự khác biệt là người chơi hiện đang ở trong một khu rừng. Bởi vì chúng tôi đang ở trong một khu rừng, không có bức tường nào. Và, nếu bạn đi bộ đủ lâu, chắc chắn bạn sẽ tìm thấy lối ra.
Vì vậy, thử thách khi chơi trò chơi này là gì? Thách thức nằm ở chỗ bây giờ chúng ta có "sức chịu đựng". Sức chịu đựng là một con số (chúng tôi bắt đầu với 10). Và, mỗi khi người chơi thực hiện một bước di chuyển, sức chịu đựng của nó sẽ giảm đi do khối lượng công việc cần thiết để vượt qua con đường hiện tại (được biểu thị bằng một số có trong hàm tạo giá trị).
Các kiểu dữ liệu và chức năng gần như giống nhau, với một số lưu ý:
- Chúng tôi không có tường.
- Chúng tôi không muốn chọn một loại số cụ thể, nhưng chúng tôi muốn đảm bảo rằng chúng tôi có thể thực hiện các thao tác số cơ bản bất kể loại mà chúng tôi chuyển đến các hàm.
- Bởi vì bây giờ chúng tôi phải theo dõi thể lực của người chơi, chúng tôi sẽ cần di chuyển nó xung quanh khu rừng hiện tại của chúng tôi. Đây sẽ là một trường hợp sử dụng tuyệt vời cho các đơn nguyên, nhưng vì chúng tôi chưa biết cách sử dụng chúng nên một cặp "(sức chịu đựng, rừng)" sẽ phải thực hiện.
Sử dụng GHCi, giống như trò chơi "Mê cung", trò chơi này sẽ trông như thế này:
*Main> giải quyếtForest testForest []
"Bạn có 10 thể lực, và bạn vẫn đang ở trong Rừng. Hãy chọn một con đường, hỡi nhà thám hiểm dũng cảm: Đi sang trái, Đi sang phải hoặc Tiến lên."
*Chính> giải quyếtForest testForest [GoForward ]
"Bạn có 7 thể lực, và bạn vẫn đang ở trong Rừng. Hãy chọn một con đường, hỡi nhà thám hiểm dũng cảm: Đi sang trái, Đi sang phải hoặc Đi tiếp."
*Chính> giải quyếtForest testForest [GoForward, GoForward]
"Bạn có 4 thể lực, và bạn vẫn đang ở trong Rừng. Hãy chọn một con đường, hỡi nhà thám hiểm dũng cảm: Đi sang trái, Đi sang phải hoặc Tiến lên."
*Main> giải quyếtForest testForest [Tiến tới, Đi tới, Đi sang trái ]
"Mày hết thể lực mà chết -.-!"
*Chính> giải quyếtForest testForest [Đi tới, Đi bên trái , Đi bên phải]
"BẠN ĐÃ TÌM ĐƯỢC LỐI RA RỒI!!"
-}
-- 1. Viết hai kiểu dữ liệu. Một cho các nước đi (Move) mà bạn có thể thực hiện, và một cho mê cung (Maze).
data Move = L | R | F  deriving (Show)
  
data Maze a =  Exit |Inside a (Maze a) (Maze a) (Maze a)  deriving (Show)

-- 2. Viết một hàm gọi là "move" nhận vào một Maze và một Move và trả về Maze sau khi di chuyển.

move:: Num a => (a,Maze a)-> Move -> (a,Maze a)
move (y,Exit) _  = (y,Exit) 
move (y,Inside a x _ _) L  = (y-a,x)
move (y,Inside a _ x _) F = (y-a,x)
move (y,Inside a _ _ x) R  = (y-a,x)


-- 3. Viết giá trị "testMaze" kiểu "Maze" và kiểm tra chức năng "move" trong GHCi.            
testMaze :: Maze Int
testMaze = Inside 2
    (Inside 4
    (Inside 1 (Inside 2 Exit (Inside 6 Exit Exit Exit) Exit) Exit Exit)
    (Inside 3 Exit (Inside 5 Exit Exit (Inside 7 Exit Exit Exit)) Exit)
    (Inside 8 Exit Exit Exit)    
    )
    (Inside 5
    (Inside 3 Exit (Inside 5 (Inside 7 Exit Exit Exit) Exit Exit) Exit)
    (Inside 4 Exit Exit Exit)
    (Inside 2 Exit (Inside 6 Exit (Inside 4 Exit Exit Exit) Exit) Exit)    
    )
    (Inside 6
    (Inside 3 Exit (Inside 2 Exit (Inside 1 Exit Exit Exit) Exit) Exit)
    (Inside 4 Exit (Inside 1 Exit Exit (Inside 1 Exit (Inside 1 Exit Exit Exit) Exit)) Exit)
    (Inside 2 Exit (Inside 1 Exit (Inside 1 (Inside 1 Exit Exit Exit) Exit Exit) Exit) Exit)    
    )


-- 4. Viết hàm "solveMaze" sẽ lấy một mê cung và danh sách các bước di chuyển rồi trả về mê cung sau khi thực hiện những động thái đó.
{-4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.
5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.
-}
-- showCurrentChoice :: Maze -> Move -> String
showCurrentChoice :: (Show a, Ord a, Num a) => (a,Maze a)-> String
showCurrentChoice (_, Exit) = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice (st, x)
            | st > 0    = "Node "++ (show x) ++" => You have " ++ (show st) ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
            | otherwise = "You ran out of stamina and died -.-!"

-- 6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D



solveForest :: (Show a, Ord a, Num a) => Maze a -> [Move] -> String
solveForest testMaze moveList = showCurrentChoice $ foldl move (10, testMaze) moveList
