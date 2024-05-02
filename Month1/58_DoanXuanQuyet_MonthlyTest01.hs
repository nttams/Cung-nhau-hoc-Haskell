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

{-
ý tưởng : 
1.tạo ra một list các phần tử chia hết cho 3 hoặc 5 
2.tính tổng list đã tạo
-}
sumMultiples :: Int -> Int
sumMultiples num = 
    let multipleList = [ x | x <- [0..(num - 1)], x `mod` 3 == 0 || x `mod` 5 == 0] :: [Int]
    in sum multipleList

{- Question 2
Write a function to calculate the sum of the digits of 2^1000

Viết hàm tính tổng các chữ số của 2^1000

KQ: 1366
-}

{-
ý tưởng : 
1.chuyển số đã nhập thành chuỗi char các ký tự
2.convert char list thành các số int
3.tính tổng các chữ số của int list
-}
sumDigits :: Integer -> Integer
sumDigits num =
    let listDigitChar = show num
        listDigitInt = map charTostring listDigitChar
                        where charTostring c = read [c] :: Integer
    in sum listDigitInt
-- sumDigits (2 ^ 1000)

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

{-
ý tưởng : 
1.check empty
2.check các input có thuộc các ký tự đã quy định
3.convert lần lượt các ký tự của input thành chuỗi kết quả
-}
toRNA :: String -> String
toRNA [] = "Empty DNAs"
toRNA dna =
    let validate = any (`notElem` "ACGT") dna
        convertDna = map convert dna
                    where convert nucleotide = case nucleotide of
                            'G' -> 'C'
                            'C' -> 'G'
                            'T' -> 'A'
                            'A' -> 'U'                    
    in  if validate then  "Invalid DNA sequence. Use only nucleotides A, C, G, or T." else convertDna

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

{-
ý tưởng : 
1.check empty
2.check input nhập có thuộc các ký tự đã quy định
3.check độ dài 2 input có bằng nhau
4.dùng hàm foldl (tạo dồn tích số điểm) giá trị khởi tạo 0 điểm
5.dùng hàm zip lặp qua 2 chuỗi để so sánh và trả về số điểm cộng tương ứng
5.1 hàm so sánh, trường hợp == cộng 1 điểm cho cả 2, trường hợp người chơi 1 thắng cộng 3 điểm, ngược lại người chơi 2 được cộng 3 điểm
-}
rockPaperScissor :: [Char] -> [Char] -> (Int, Int)
rockPaperScissor [] [] = (0,0)
rockPaperScissor player1 player2
    | validate1 || validate2 || validate3 = (0, 0)
    | otherwise = result
    where
        validate1 = any (`notElem` "PRS") player1
        validate2 = any (`notElem` "PRS") player2
        validate3 = length player1 /= length player2
        result = foldl compareElements (0, 0) (zip player1 player2)
        compareElements (player1_score, player2_score) (playA, playB)
            | playA == playB = (player1_score + 1, player2_score + 1)
            | (playA, playB) `elem` [('R', 'S'), ('S', 'P'), ('P', 'R')] = (player1_score + 3, player2_score)
            | otherwise = (player1_score, player2_score + 3)