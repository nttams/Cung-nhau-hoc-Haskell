import System.Directory  
import System.FilePath ((</>))  
import Control.Monad (when)  
import System.IO (IOMode(ReadMode), hClose, hIsEOF, openFile, hGetLine, withFile, hGetContents) 
import Data.List (isPrefixOf) 
import Text.Read (readMaybe)  
import Data.Char (isDigit)  
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.List (sort)
import Control.Monad (when, unless)

{- Question 1
Khi tao 1 NFT theo CIP-25, chung ta can dinh kem metadata vao giao dich.
Mot metadata don gian chua 1 so thong tin sau:

{
    "721": {
        "<policy_ID>": {
            "<asset_name>": {
                "description": "<desc>",
                "name": "<asset_name>",  -- Ten hien thi cua NFT, tren thuc te, khong can giong <asset_name>
                "image": "img url"
            }
        }
    }
}

Viet ham yeu cau nguoi dung nhap vao 1 so thong tin: <policy_ID>, <asset_name>, <desc>
Tu do tao va ghi metadata vao file metadata.json
-}

createMetadata :: IO ()
createMetadata = do
    putStrLn "Nhap policy ID:"
    policyID <- getLine
    putStrLn "Nhap ten tai san:"
    assetName <- getLine
    putStrLn "Nhap mo ta:"
    desc <- getLine
    putStrLn "Nhap URL hinh anh:"
    imageUrl <- getLine

    let metadata = "{\n"
                ++ "    \"721\": {\n"
                ++ "        \"" ++ policyID ++ "\": {\n"
                ++ "            \"" ++ assetName ++ "\": {\n"
                ++ "                \"description\": \"" ++ desc ++ "\",\n"
                ++ "                \"name\": \"" ++ assetName ++ "\",\n"
                ++ "                \"image\": \"" ++ imageUrl ++ "\"\n"
                ++ "            }\n"
                ++ "        }\n"
                ++ "    }\n"
                ++ "}\n"

    writeFile "metadata.json" metadata
    putStrLn "Da tao tep metadata.json"

{- Q2
Viet ham thuc hien cac nhiem vu sau:
    - Tao 1 thu muc con trong thu muc hien tai. 
    - Di chuyen file tao o bai tren vao thu muc con
    - Doi ten file
    - Xoa file
    - Xoa thu muc con
Sau moi thao tac tren, in ra cay thu muc
-}

-- Function to print the directory tree (from homework week11)

printDirectoryTree :: FilePath -> IO ()
printDirectoryTree path = do
  putStrLn path
  printEntry "" path

-- In cac thu muc va tep trong thu muc hien tai
printEntry :: String -> FilePath -> IO ()
printEntry prefix currentDir = do
  filePaths <- sort <$> listDirectory currentDir
  case filePaths of
    [] -> return ()
    [singleFile] -> printTree prefix currentDir (singleFile, True)
    _ -> do
      mapM_ (printTree prefix currentDir) (zip (init filePaths) (repeat False))
      printTree prefix currentDir (last filePaths, True)

-- In 
printTree :: String -> FilePath -> (FilePath, Bool) -> IO ()
printTree prefix currentDir (path, isLast) = do
  let fullPath = currentDir </> path
  isDir <- doesDirectoryExist fullPath
  if isLast
    then putStrLn $ prefix ++ "└── " ++ path
    else putStrLn $ prefix ++ "├── " ++ path
  when isDir $ do
    let newPrefix = if isLast then prefix ++ "    " else prefix ++ "│   "
    printEntry newPrefix fullPath

-- Thuc hien cac thao tac voi tep va thu muc
performFileOperations :: IO ()
performFileOperations = do
  let subDir = "subdirectory"
      originalFile = "metadata.json"
      newFile = "renamed_metadata.json"
  
  -- Tao thu muc con neu chua ton tai
  dirExists <- doesDirectoryExist subDir
  unless dirExists $ createDirectory subDir
  putStrLn "Da tao hoac kiem tra ton tai thu muc con:"
  printDirectoryTree "."
  
  -- Di chuyen tep vao thu muc con
  renameFile originalFile (subDir </> originalFile)
  putStrLn "Da di chuyen tep vao thu muc con:"
  printDirectoryTree "."
  
  -- Doi ten tep
  renameFile (subDir </> originalFile) (subDir </> newFile)
  putStrLn "Da doi ten tep:"
  printDirectoryTree "."
  
  -- Xoa tep
  removeFile (subDir </> newFile)
  putStrLn "Da xoa tep:"
  printDirectoryTree "."
  
  -- Xoa thu muc con
  removeDirectory subDir
  putStrLn "Da xoa thu muc con:"
  printDirectoryTree "."


{- Question 3
Khi dung cardano-cli de query cac UTxO tren 1 dia chi, chung ta nhan duoc ket qua dang nhu sau:

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1f845ad67df909fcea522c7ff0d3c73ff17ada2436c1d65314acfd532e9789f0     0        9826876 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     0        33000000 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     1        66177669 lovelace + TxOutDatumNone
d88ba55e07f9c8a091b87f2550aa97b331f05c0618bbbf8b43cf186ea38de3b8     0        4829226 lovelace + TxOutDatumNone
dd30de76b03db635eae4de221678a5190866f32f692598c87cefb8401e8ae7fb     0        4829226 lovelace + TxOutDatumNone

Cac thong tin nay dang duoc luu trong file ./utxo.txt

Hay viet ham `selectUtxo` thuc hien cac viec sau:
- Doc noi dung file utxo.txt, bo 2 dong dau, in ra cac dong con lai kem theo so thu tu cua chung tinh tu 1. 
- Yeu cau nguoi dung lua chon 1 utxo theo so thu tu. 
    Kiem tra tinh hop le cua so nhap vao (nam trong khoang hop le dua tren so luong utxo)
- In ra thong tin cua utxo duoc chon bao gom: ma tham chieu (co dang <TxHash>#<TxIx>) va so lovelace chua trong no

De ro hon, Hay xem VD:

ghci> selectUtxo utxoFile 
Reading file ./utxo.txt
Utxo list:
1: 1f845ad67df909fcea522c7ff0d3c73ff17ada2436c1d65314acfd532e9789f0     0        9826876 lovelace + TxOutDatumNone
2: ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     0        33000000 lovelace + TxOutDatumNone
3: ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     1        66177669 lovelace + TxOutDatumNone
4: d88ba55e07f9c8a091b87f2550aa97b331f05c0618bbbf8b43cf186ea38de3b8     0        4829226 lovelace + TxOutDatumNone
5: dd30de76b03db635eae4de221678a5190866f32f692598c87cefb8401e8ae7fb     0        4829226 lovelace + TxOutDatumNone
Please enter the line number of utxo you want to select:
aa
Invalid input. Please enter a number:
123k
Invalid input. Please enter a number:
0
Invalid number. Please enter a line number in valid range:
10
Invalid number. Please enter a line number in valid range:
2
You select utxo ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42#0 with 33000000 lovelace.
-}

selectUtxo :: FilePath -> IO ()
selectUtxo filePath = do
    contents <- readFile filePath
    let utxos = drop 2 (lines contents)
    putStrLn "Utxo list:"
    mapM_ putStrLn [show idx ++ ": " ++ utxo | (idx, utxo) <- zip [1..] utxos]
    
    putStrLn "Please enter the line number of utxo you want to select:"
    selectLoop utxos

selectLoop :: [String] -> IO ()
selectLoop utxos = do
    input <- getLine
    case readMaybe input of
        Just n | n > 0 && n <= length utxos -> do
            let selectedUtxo = utxos !! (n - 1)
            let parts = words selectedUtxo
            let txHash = parts !! 0
            let txIx = parts !! 1
            let amount = parts !! 2
            putStrLn $ "You select utxo " ++ txHash ++ "#" ++ txIx ++ " with " ++ amount ++ " lovelace."
        _ -> do
            putStrLn "Invalid input. Please enter a valid number."
            selectLoop utxos

{- Q4:
Viết hàm sumNumbers yêu cầu người dùng nhập vào một danh sách các số nguyên,
(mỗi số trên 1 dòng, bỏ qua các dòng không phải là số nguyên, kết thúc nhập khi nhận được 1 dòng trống) 
và sau đó in ra tổng của các số đó. Lưu ý: hàm cần chấp nhận cả số âm

Ví dụ:
ghci> sumNumbers 
Enter numbers (one per line), and an empty line to end:
1
2
20

The sum is: 23


ghci> sumNumbers 
Enter numbers (one per line), and an empty line to end:
-10
3a
abcd
--30
110

The sum is: 100
-}

sumNumbers :: IO ()
sumNumbers = do
    putStrLn "Enter numbers (one per line), and an empty line to end:"
    sumLoop 0

sumLoop :: Int -> IO ()
sumLoop acc = do
    input <- getLine
    if null input
        then putStrLn $ "The sum is: " ++ show acc
        else case readMaybe input :: Maybe Int of
            Just n -> sumLoop (acc + n)
            Nothing -> sumLoop acc
                  
