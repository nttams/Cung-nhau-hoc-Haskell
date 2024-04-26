{-
Viết password generator, với các yêu cầu sau
1. Nhập vào 2 thông số, trả kết quả ra String
+ Site name (String): ví dụ như "google", "telegram", "facebook", đây là tên của trang web hay app cần tạo mật khẩu
+ Version (Int): một số trang web bắt buộc phải đổi password sau 1 khoảng thời gian, hoặc bạn bị lộ mật khẩu cần phải tạo mới. 


2. Mật khẩu giữa các trang khác nhau NÊN (không phải bắt buộc) khác nhau
3. Mỗi lần tăng version, thì mật khẩu phải thay đổi đáng kế (tất nhiên k thể mạnh như Hashing được)

---------
Ví dụ

Cần 2 bảng quy ước
(1) quy ước ký tự ra số thứ tự: 'a' = 0, 'b' = 1, ..., 'z'=25
(2) quy ước ký tự ra từ ngẫu nhiên:
    'a' -> "apple"
    'b' -> "banana"
    'c' -> "cat"
    'd' -> "dog"
    'e' -> "enum"
    'f' -> "fire"
    'g' -> "game"
    'h' -> "high"
    'i' -> "internet"
    'j' -> "jet"
    'k' -> "know"
    'l' -> "lion"
    'm' -> "monkey"
    'n' -> "nope"
    'o' -> "octopus"
    'p' -> "pen"
    'q' -> "quota"
    'r' -> "rhino"
    's' -> "sleep"
    't' -> "tiger"
    'u' -> "under"
    'v' -> "vampire"
    'w' -> "whale"
    'x' -> "xeon"
    'y' -> "yogurt"
    'z' -> "zebra"

Ví dụ 1: Cần tạo mật khẩu cho trang "google", version=1
- B1: lấy ký tự đầu là 'g' tra bảng 1 được 6, cộng cho version (=1) = 7, từ số 7 tra bảng 1 được ký tự 'h', tra tiếp bảng 2 được chữ "high"
- B2: lấy ký tự thứ 2 là 'o' tra bảng 1 được 14, cộng cho version+1 (=2) = 16, từ số 16 tra bảng 1 được ký tự 'q', tra tiếp bảng 2 được chữ "quota"
- B3: lấy ký tự cuối cùng là 'e', tra bảng 1 được 4, nhân cho version rồi + version lần nữa = 5
- B4: ghép lại ta được: "high" + "quota" + "5" = "highquota5"
- B5: có thể thêm prefix hoặc postfix bất kỳ để thêm phần phức tạp: "prefix_highquota5_@"

Ví dụ 2: Cần tạo mật khẩu cho trang "google", version=2
- B1: lấy ký tự đầu là 'g' tra bảng 1 được 6, cộng cho version (=2) = 8, từ số 8 tra bảng 1 được ký tự 'i', tra tiếp bảng 2 được chữ "internet"
- B2: lấy ký tự thứ 2 là 'o' tra bảng 1 được 14, cộng cho version+1 (=3) = 17, từ số 17 tra bảng 1 được ký tự 'r', tra tiếp bảng 2 được chữ "rhino"
- B3: lấy ký tự cuối cùng là 'e', tra bảng 1 được 4, nhân cho version rồi + version lần nữa = 4 * 2 + 2 = 10
- B4: ghép lại ta được: "internet" + "rhino" + "10" = "internetrhino10"
- B5: có thể thêm prefix hoặc postfix bất kỳ để thêm phần phức tạp: "prefix_internetrhino10_@"

---------

Tóm tắt: 
"google", version1 -> "prefix_highquota5_@"
"google", version2 -> "prefix_internetrhino10_@"

Mấu chốt ở đây là phải tạo được bảng 2 bằng các từ mà mọi người có thể nhớ được, và có thể đơn giản hoá/ hoặc phức tạp cách tính tuỳ sở thích của mỗi người. Hiện tại thì em cũng dùng cách này để nhớ mật khẩu (tất nhiên công thức k giống và bảng 2 cũng không giống)
-}


import Data.Char (ord, chr, toLower)
-- ASCII
-- ord: Char -> Int
-- chr: Int -> Char



getWord :: Char -> String
getWord c = case c of
    'a' -> "apple"
    'b' -> "banana"
    'c' -> "cat"
    'd' -> "dog"
    'e' -> "enum"
    'f' -> "fire"
    'g' -> "game"
    'h' -> "high"
    'i' -> "internet"
    'j' -> "jet"
    'k' -> "know"
    'l' -> "lion"
    'm' -> "monkey"
    'n' -> "nope"
    'o' -> "octopus"
    'p' -> "pen"
    'q' -> "quota"
    'r' -> "rhino"
    's' -> "sleep"
    't' -> "tiger"
    'u' -> "under"
    'v' -> "vampire"
    'w' -> "whale"
    'x' -> "xeon"
    'y' -> "yogurt"
    'z' -> "zebra"
    otherwise -> "ERROR"

getIndexOfChar :: Char -> Int
getIndexOfChar c = ord c - 97

getCharFromIndex :: Int -> Char
getCharFromIndex i = chr (i + 97)

getFirstPart :: String -> Int -> String
getFirstPart name version =
    let chr = name !! 0
        index = getIndexOfChar chr
        nextIndex = index + version
        newChar = getCharFromIndex nextIndex
    in getWord newChar

getSecondPart :: String -> Int -> String
getSecondPart name version =
    let chr = name !! 1
        index = getIndexOfChar chr
        nextIndex = index + version + 1
        newChar = getCharFromIndex nextIndex
    in getWord newChar

getThirdPart :: String -> Int -> Int
getThirdPart name version =
    let chr = reverse name !! 0
        index = getIndexOfChar chr
    in index * version + version

generatePassword :: String -> Int -> String
generatePassword name version
    | length name < 3 = "name must have length >= 3"
    | version < 0 = "version must be positive"
    | otherwise = "prefix_" ++
                  getFirstPart name version ++
                  getSecondPart name version ++
                  show (getThirdPart name version) ++
                  "_@"