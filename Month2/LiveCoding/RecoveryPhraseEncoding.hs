-- 24 character encoding for Cardano Wallet Recovery Phrase 


list1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]


list2 = ["Apple", "Banana","Cherry", "Date", "Elderberry", "Fig", "Grape", "Honeydew", "Kiwi", "Lemon", "Mango", "Nectarine", "Orange", "Papaya", "Quince", "Raspberry", "Strawberry", "Tangerine", "Ugli Fruit", "Watermelon", "Blueberry", "Pineapple", "Coconut", "Lychee"]


reverseGroupN :: Int->[a]->[a]
reverseGroupN _ [] = []
reverseGroupN n xs = reverse(take n xs) ++ reverseGroupN n (drop n xs)


groupSizes :: [Int]
groupSizes = [2, 3, 4, 6, 8, 12]


noChange :: a -> a
noChange x = x



enCode :: [a] -> [a]
enCode = reverse.foldr (.) noChange (map reverseGroupN groupSizes)


dEnCode :: [a] -> [a]
dEnCode = foldr (.) noChange (map reverseGroupN (reverse groupSizes)).reverse





listChar = ['a'..'z']++['A'..'Z']
listNum = [1000,10012..]




rules = zip listChar listNum

chartToNum :: Char->[Int]
chartToNum c = foldr (\(x,y) acc->if x==c then y : acc else acc ) [] rules
-- chartToNum c  = [y|(x,y)<-rules,x==c]

stringToListNum :: String->[Int]
stringToListNum = foldr (\x acc->acc ++ chartToNum x) []
-- stringToListNum = concatMap chartToNum


listStringToListNum :: [String]->[[Int]]
listStringToListNum = foldr (\x acc->stringToListNum x : acc) []

-- listStringToListNum = map stringToListNum



enCode' :: [String]->[[Int]]
enCode' = listStringToListNum.enCode


numToChar :: Int->[Char]
numToChar n = foldr (\(x,y) acc-> if y==n then x:acc else acc) [] rules

listNumToString :: [Int]->String
listNumToString = foldr (\x acc-> acc++ numToChar x) []

listNumToListString :: [[Int]]->[String]
listNumToListString = foldr (\x acc->listNumToString x :acc) []


dEnCode' :: [[Int]]->[String]
dEnCode'  = dEnCode.listNumToListString