
{- Question 1 -}
-- Xuất phát từ binary search tree với kiểu Tree trong bài học
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

threeLevelTree :: Tree Integer
threeLevelTree = Node 8
                    (Node 3
                      (Node 1 Empty Empty)
                      (Node 6 Empty Empty)
                    )
                    (Node 10
                      Empty
                      (Node 14 Empty Empty)
                    )

{- 1a) Viết hàm `insert` để thêm một phần tử vào một binary search tree có sẵn. 
       Nếu phần tử đã có trong cây rồi thì không thêm.
 ví dụ
ghci> insert threeLevelTree 0
Node 8 (Node 3 (Node 1 (Node 0 Empty Empty) Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))

ghci> insert threeLevelTree 10
Node 8 (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))
-}

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty v = Node v Empty Empty
insert (Node x l r) v
    | v < x = Node x (insert l v) r
    | v > x = Node x l (insert r v)
    | otherwise = Node x l r

{- 1b) Từ hàm trên, viết hàm `insertList` để  thêm 1 list các phần tử vào 1 cây binary search tree 

ghci> insertList threeLevelTree [1,2,5,20]
Node 8 (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 6 (Node 5 Empty Empty) Empty)) (Node 10 Empty (Node 14 Empty (Node 20 Empty Empty)))

ghci> insertList Empty [1,2,1,5,5,20]
Node 1 Empty (Node 2 Empty (Node 5 Empty (Node 20 Empty Empty)))
-}
insertList :: (Ord a) => Tree a -> [a] -> Tree a
insertList = foldl insert

{-  1c) Sử dụng hàm đã tạo ở trên để viết hàm `treeSort` với ý tưởng sau:
        - Nhận vào một danh sách các phần tử.
        - Chuyển tất cả các phần tử trong danh sách vào một cây nhị phân.
        - Chuyển lại cây nhị phân thành một danh sách với các phần tử được sắp xếp theo thứ tự tăng dần. 
    (Với hàm này, các phần tử trùng lặp cũng sẽ được loại bỏ khỏi danh sách kết quả)

ví dụ
  ghci> treeSort  [1,3,10,100,3,25,1]
  [1,3,10,25,100]

  ghci> treeSort  ['r','e','1','b','a','b','e']
  "1aber"

  ghci> treeSort "cardano2vn"
  "2acdnorv"
-}

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node v l r) = treeToList l ++ (v : treeToList r)

treeSort :: (Ord a) => [a] -> [a]
treeSort = treeToList . insertList Empty

{- Question 2 
Viết một hàm nhận vào một Binary Search Tree và giá trị của hai nút A và B, 
kiểm tra xem có tồn tại đường đi từ A đến B không. 
Lưu ý: chúng ta chỉ có thể đi theo một chiều từ nút cha đến nút con. Theo hình minh họa nó là chiều từ trên xuống.

          10
        /    \
       5      15
      / \    /  \
     3   7  13  20
    / \   \       \
   1   4   8      24

ghci> hasPath testTree 15 24
True
ghci> hasPath testTree 10 3
True
ghci> hasPath testTree 10 25
False
ghci> hasPath testTree 15 5
False
ghci> hasPath (insertList Empty "haskell-club") 'a' 'c'
True
-}
testTree :: Tree Integer
testTree = insertList Empty [10,5,15,3,7,13,20,1,4,8,24]

isIn :: (Ord a) => Tree a -> a -> Bool
isIn Empty _= False
isIn (Node v l r) x
    | x < v = isIn l x
    | x > v = isIn r x
    | otherwise = x == v

hasPath :: (Ord a) => Tree a -> a -> a -> Bool
hasPath Empty _ _ = False
hasPath (Node v l r) v1 v2
    | v1 < v = hasPath l v1 v2
    | v1 > v = hasPath r v1 v2
    | otherwise = isIn (Node v l r) v2



{- Question 3 -}
-- Chúng ta có kiểu `Expr` biểu diễn các biểu thức số học với các phép toán Cộng, Trừ, Nhân, Chia và các Hằng số nguyên.
data Expr = Const Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

-- Ví dụ: 
expr1 :: Expr
expr1 = Add (Const 5) 
            (Mul (Const 2) 
                 (Sub (Const 3) 
                      (Const 10)))
-- biểu thị biểu thức: 5 + (2 * (3 - 10))
expr2 :: Expr
expr2 = Mul (Add (Const 5)
                 (Const 10)) 
            (Div (Const 36) 
                 (Add (Const (-1)) 
                      (Const 7)))
-- (5 + 10) * ( 36 `div` ( (-1) + 7 ) )

{- 3a  Viết hàm tính giá trị của biểu thức được khai báo bằng kiểu trên

ghci> evalExpr expr1
-9
ghci> evalExpr expr2
90
-}
evalExpr :: Expr -> Integer
evalExpr (Const v) = v
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2

{- 3b Chúng ta nhận thấy hàm trên sẽ gặp lỗi với trường hợp chia cho 0: 
    ghci> evalExpr $ Div (Const 1) (Const 0)
    *** Exception: divide by zero

Trong những tình huống như này chúng ta có thể dùng kiểu `Maybe` để xử lý.
Định nghĩa của nó giống như kiểu `Box` trong bài học:
    data Maybe a = Nothing | Just a
Chính xác hơn thì `Maybe` là 1 type contructor. 
Giá trị `Maybe a` thể hiện khả năng một biểu thức có thể trả về hoặc không trả về một giá trị hợp lệ (optional values)

Hãy dùng `Maybe` đê viết lại hàm `evalExpr` trên, để  có thể sử dụng nó an toàn cho cả trường hợp chia cho 0

  ghci> evalExprMaybe  $ Div (Const 200) (Const 0)
  Nothing
  ghci> evalExprMaybe  $ Div (Const 200) (Const 10)
  Just 20
  ghci> evalExprMaybe expr1
  Just (-9)
  ghci> evalExprMaybe expr2
  Just 90
--}

evalExprMaybe :: Expr -> Maybe Integer
evalExprMaybe (Const v) = Just v
evalExprMaybe (Add e1 e2) =
    case (evalExprMaybe e1, evalExprMaybe e2) of
         (Just v1, Just v2) -> Just(v1 + v2)
         _ -> Nothing
evalExprMaybe (Sub e1 e2) =
    case (evalExprMaybe e1, evalExprMaybe e2) of
         (Just v1, Just v2) -> Just(v1 - v2)
         _ -> Nothing
evalExprMaybe (Mul e1 e2) =
    case (evalExprMaybe e1, evalExprMaybe e2) of
         (Just v1, Just v2) -> Just(v1 * v2)
         _ -> Nothing
evalExprMaybe (Div e1 e2) =
    case (evalExprMaybe e1, evalExprMaybe e2) of
         (Just v1, Just v2) | v2 /= 0 -> Just(v1 `div` v2)
         _ -> Nothing

{- Question 4: 
Viết một hàm kiểm tra xem nước đi của 1 quân cờ vua trên bàn cờ trống có hợp lệ hay không.
Hàm nhận vào loại quân cờ, vị trí hiện tại của nó trên bàn cờ, và vị trí mới (vị trí được biểu thị bằng tọa độ hàng và cột).
Rồi trả về  True nếu nước đi đến vị trí mới hợp lệ, False nếu không hợp lệ.

Để đơn giản, chúng ta chỉ xét đến 3 loại quân sau:
  Quân Vua (King)
  Quân Xe (Rook)
  Quân Mã (Knight)

Yêu cầu:
  Định nghĩa các kiểu dữ liệu cần thiết:
    Kiểu dữ liệu đại diện cho các loại quân cờ: `PieceType`
    Kiểu dữ liệu đại diện cho vị trí trên bàn cờ (hàng, cột): `Position`
      
  Viết hàm isValidMove với chữ ký như sau:
    isValidMove :: PieceType -> Position -> Position -> Bool
  Ngoài ra, trước tiên, hàm cũng cần check các tọa độ truyền vào có hợp lệ hay không, với hàng và cột là các số nguyên từ 0 -> 7

Ví dụ:
ghci> isValidMove Rook (0, 0) (0, 5)
True
ghci> isValidMove Rook (2, 5) (0, 5)
True
ghci> isValidMove Rook (1, 0) (1, 8)
False

ghci> isValidMove King (4, 4) (5, 4)
True
ghci> isValidMove King (4, 4) (5, 5)
True
ghci> isValidMove King (4, 4) (5, 6)
False

ghci> isValidMove Knight (7, 1) (5, 2)
True
ghci> isValidMove Knight (7, 1) (8, 3)
False
ghci> isValidMove Knight (7, 1) (6, 3)
True
-}

data PieceType = King | Rook | Knight
type Position = (Int, Int)

isValidPosition :: Position -> Bool
isValidPosition (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

isValidMove :: PieceType -> Position -> Position -> Bool
isValidMove _ p1 p2 | not (isValidPosition p1 && isValidPosition p2) = False
isValidMove Rook (x1, y1) (x2, y2) =
    (x1 == x2 && y1 /= y2) || -- move on same row
    (x1 /= x2 && y1 == y2)    -- move on same column
isValidMove King (x1, y1) (x2, y2) =
    ((x1 /= x2) || (y1 /= y2)) && -- cannot move to the same position
    abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1
isValidMove Knight (x1, y1) (x2, y2) =
    (deltaX == 2 && deltaY == 1) ||
    (deltaX == 1 && deltaY == 2)
    where
        deltaX = abs(x1 - x2)
        deltaY = abs(y1 - y2)