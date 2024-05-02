{- Question 1
If we list all the positive integers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Write a function that takes a positive integer n and computes the sum of all multiples of 3 or 5 below n.

Nếu liệt kê tất cả các số nguyên dương nhỏ hơn 10 là bội số của 3 hoặc 5 thì ta được 3, 5, 6 và 9.Tổng của các bội số này là 23.
Viết hàm nhận vào số nguyên dương n, rồi tính tổng của tất cả các bội số nhỏ hơn n của 3 hoặc 5.

Example:
ghci> sumMultiples 10
23
ghci> sumMultiples 1000
233168
ghci> sumMultiples 100
2318
-}

-- let a = [x | x <- [1..], x `mod` 3 == 0 || x `mod` 5 == 0]

sumMultiples :: Int -> Int
sumMultiples n = sum [x | x <- [1 .. n - 1], x `mod` 3 == 0 || x `mod` 5 == 0]

{- Question 2
Write a function to calculate the sum of the digits of 2^1000

Viết hàm tính tổng các chữ số của 2^1000

KQ: 1366
-}
sumOfDigits :: Integer -> Int
sumOfDigits 0 = 0 
sumOfDigits n = fromIntegral (n `mod` 10) + sumOfDigits (n `div` 10)

sumOfDigitsOfPower :: Int
sumOfDigitsOfPower = sumOfDigits (2^1000)

{- Question 3
You work for a bioengineering company that specializes in developing therapeutic solutions.

Your task is determine the RNA complement of a given DNA sequence.

Both DNA and RNA strands are a sequence of nucleotides.

The four nucleotides found in DNA are adenine (A), cytosine (C), guanine (G) and thymine (T).
The four nucleotides found in RNA are adenine (A), cytosine (C), guanine (G) and uracil (U).

Given a DNA strand, its transcribed RNA strand is formed by replacing each nucleotide with its complement:
    G -> C
    C -> G
    T -> A
    A -> U

Write a function that takes a DNA sequence and returns the corresponding RNA sequence.

Bạn đang làm việc cho một công ty công nghệ sinh học chuyên phát triển các giải pháp trị liệu.

Nhiệm vụ của bạn là xác định chuỗi RNA bổ sung cho một chuỗi DNA đã cho.

Cả các chuỗi DNA và RNA đều là một chuỗi các nucleotide.

Có bốn loại nucleotide trong DNA: adenine (A), cytosine (C), guanine (G), và thymine (T).

Có bốn loại nucleotide trong RNA: adenine (A), cytosine (C), guanine (G), và uracil (U).

Cho một chuỗi DNA, chuỗi RNA phiên mã được hình thành bằng cách thay thế mỗi nucleotide trong chuỗi DNA bằng nucleotide bổ sung của nó:
    G -> C
    C -> G
    T -> A
    A -> U

Viết hàm nhận vào chuỗi DNA và trả về chuỗi RNA phiên mã, kiểm tra các trường hợp riêng của DNA đầu vào (chuỗi rỗng, chứa nucleotide không đúng)
    toRNA :: String -> String

Example:
> toRNA ""
"Empty DNAs"
> toRNA "ACGTQW"
"Invalid DNA sequence. Use only nucleotides A, C, G, or T."
> toRNA "G"
"C"
> toRNA "ACGTGGTCTTAA"
"UGCACCAGAAUU"

-}

convertRNA :: Char -> String

convertRNA x 
  | x == 'G' = ['C']
  | x == 'C' = ['G']
  | x == 'T' = ['A']
  | otherwise = ['U']

toRNA :: String -> String
toRNA [] = "Empty DNAs"
toRNA [x] = convertRNA x
toRNA (x : xs)
  | x == 'G' || x == 'C' || x == 'T' || x == 'A' = convertRNA x ++ toRNA xs
  | otherwise = error " Invalid DNA sequence. Use only nucleotides A, C, G, or T. "

{- Question 4
Rock-Paper-Scissors Game
Use characters to represent the moves:
    'R': Rock
    'P': Paper
    'S': Scissors
The score for each player in one round is as follows:
    Winner: 3 points
    Loser: 0 points
    Draw: Each person gets 1 point
There are two players, A and B. Write a function
    rockPaperScissor :: [Char] -> [Char] -> (Int, Int)
that takes two lists representing the moves of A and B.
Returns the result as a tuple containing the total score of A and the total score of B.

Trò chơi Oẳn tù tì
Sử dụng các ký tự đại diện cho nước đi:
    'R': Đá
    'P': Giấy
    'S': Kéo
Điểm số cho từng người trong 1 lần chơi như sau:
    Người thắng: 3 điểm
    Người thua:  0 điểm
    Hòa: Mỗi người 1 điểm
Có 2 người chơi A và B. Viết hàm
    rockPaperScissor :: [Char] -> [Char] -> (Int, Int)
nhận vào 2 list thể hiện nước đi của A, B.
Trả về kết quả là 1 tuple chứa tổng điểm của A và tổng điểm của B.

Example:
> rockPaperScissor "RRR" "PPP"
(0,9)
> rockPaperScissor "RSRPP" "PSSSP"
(5,8)
> rockPaperScissor "RSSP" "SPRP"
(7,4)
-}

playerScore :: Char -> Char -> Int
playerScore player y
    | player == y = 1
    | (player == 'R' && y == 'S') || (player == 'S' && y == 'P') || (player == 'P' && y == 'R') = 3
    | otherwise = 0

rockPaperScissor :: [Char] -> [Char] -> (Int, Int)
rockPaperScissor [] [] = (0, 0)
rockPaperScissor (x:xs) (y:ys) = (player1 + a, player2 + b)
    where
        player1 = playerScore x y
        player2 = playerScore y x
        (a, b) = rockPaperScissor xs ys