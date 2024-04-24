# LIST COMPREHENSION
Cung cấp 1 cách ngắn gọn để tạo và xử lý list

Trong toán, **SET COMPREHENSION** sử dụng để tạo 1 tập hợp bắt nguồn 1 tập hợp có sẵn.

Ví dụ: tập hợp 10 số tự nhiên chẵn đầu tiên có thể viết như sau
$$\{2x | x ∈ N, x < 10\}$$

-> **LIST COMPREHENSION** trong Haskell:
```haskell
[x*2 | x <- [0..9]]
```

List 10 số chính phương chẵn đầu tiên:
```haskell
take 10 [ x*x | x <- [1..], even x ]
```

Các số chia 7 dư 3,  từ 50 -> 100, hoặc 150 -> 200
```haskell
[ x | x <- [50..100] ++ [150..200], x `mod` 7 == 3]
```

## Cú pháp
List comprehensions have three main parts:
- Any number of input lists
- Any number of predicates (conditions)
- A single output function
```
[ <outputFunction> | <inputList>, <predicate> ]
```

`inputList` truyền các phần tử của nó thỏa mãn điều kiện `predicate` lần lượt vào `outputFunction`.

`predicate` là hàm kiểm tra một phần tử và trả về `True`/`False`.

Nhiều điều kiện:
```haskell
[x | x <- [1..80], x `mod` 4 == 0, x `mod` 6 == 0]
```

```haskell
meals = [("Burger", 3, 310), ("Pizza", 5, 340), ("Ramen", 2, 250)]

filteredMeals = [name | (name, price, calories) <- meals, price < 4, calories > 300]
```

Nhiều input list
```haskell
[(x,y) | x <- [1..3], y <- [1..4]]
[(x,y) | x <- [1..3], y <- [x..4]]
```

```haskell
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss , x <- xs]
```

Tạo list lồng nhau
```haskell
[ [x | x <- [1..y]] | y <- [3..5] ]
```

Chuỗi:
```haskell
func1 st = [ c | c <- st, c `elem` ['A'..'Z']]
```

let binding
```haskell
[double | x <- [1..10], let double = x*2] 
```
```haskell
[(x,y) | x <- [1..4], let y=x*x+1, even y]
```

Tìm các số nguyên tố <= n
```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
```



*Viết lại hàm length*
```haskell
    length' :: [a] -> Int
```
*Đếm số lần xuất hiện ký tự x trong chuỗi xs*
```haskell	
    count :: Char -> String -> Int
```


## Hàm zip 
zip takes two lists and returns a list of corresponding pairs.
```
>>> zip [1, 2] ['a', 'b']
[(1,'a'),(2,'b')]
```
If one input list is shorter than the other, excess elements of the longer list are discarded, even if one of the lists is infinite:
```
>>> zip [1] ['a', 'b']
[(1,'a')]   


>>> zip [1, 2] ['a']```
[(1,'a')]

>>> zip [1] [1..]
[(1,1)]

>>> zip [1..] []
[]
```

Kiểm tra list các số nguyên có sắp xếp tăng dần không:
```haskell
sorted :: [Int] -> Bool
sorted xs = and [x<=y | (x,y) <- zip xs (tail xs) ]
```

*Trả về tất cả các vị trí xuất hiện ký tự x trong chuỗi xs:*
```haskell
    positions :: Char -> String -> [Int]
```