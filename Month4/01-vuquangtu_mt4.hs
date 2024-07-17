import           Data.List        (foldl')

import           System.Directory
import           System.FilePath

import           Text.Read        (readMaybe)

{- Question 1
Khi tạo 1 NFT theo CIP-25, chúng ta cần đính kèm metadata vào giao dịch.
Một metadata đơn giản chứa 1 số thông tin sau:

{
    "721": {
        "<policy_ID>": {
            "<asset_name>": {
                "description": "<desc>",
                "name": "<asset_name>",  -- Tên hiển thị của NFT, trên thực tế, không cần giống <asset_name>
                "image": "img url"
            }
        }
    }
}



Viết hàm yêu cầu người dùng nhập vào 1 số thông tin: <policy_ID>, <asset_name>, <desc>
Từ đó tạo và ghi metadata vào file metadata.json


-}
makeMetadata :: String -> String -> String -> String
makeMetadata policyID assetName desc =
  "{\n"
    ++ "    \"721\": {\n"
    ++ "        \""
    ++ policyID
    ++ "\": {\n"
    ++ "            \""
    ++ assetName
    ++ "\": {\n"
    ++ "                \"description\": \""
    ++ desc
    ++ "\",\n"
    ++ "                \"name\": \""
    ++ assetName
    ++ "\",\n"
    ++ "                \"image\": \""
    ++ "imgURL"
    ++ "\"\n"
    ++ "            }\n"
    ++ "        }\n"
    ++ "    }\n"
    ++ "}\n"

metadataMenu :: IO ()
metadataMenu = do
  putStrLn "Enter the <policy_ID>"
  policyID <- getLine
  putStrLn "Enter the <asset_name>"
  assetName <- getLine
  putStrLn "Enter the <desc>"
  desc <- getLine
  let metadata = makeMetadata policyID assetName desc
  writeFile "metadata.json" metadata

{- Q2
Viết hàm thực hiện các nhiệm vụ sau:
    - Tạo 1 thư mục con trong thư mục hiện tại.
    - Di chuyển file tạo ở bài trên vào thư mục con
    - Đổi tên file
    - Xóa file
    - Xóa thư mục con
Sau mỗi thao tác trên, in ra cây thư mục bằng hàm đã tạo trong homework week11, để xem kết quả
-}
printFolderTree :: FilePath -> IO ()
printFolderTree currentPath = do
  let currentFolderName = takeFileName currentPath
  putStrLn "\n Current Folder Tree"
  putStrLn $ show currentFolderName
  printEntry "" currentPath
  where
    printEntry :: String -> FilePath -> IO ()
    printEntry prefix currentPath = do
      filePaths <- listDirectory currentPath
      case filePaths of
        [] -> return ()
        _ -> do
          mapM_
            (printTree prefix currentPath)
            (zip (init filePaths) (repeat False))
          mapM_ (printTree prefix currentPath) [(last filePaths, True)]
    printTree :: String -> FilePath -> (FilePath, Bool) -> IO ()
    printTree prefix currentPath (path, isLast) = do
      isDir <- doesDirectoryExist path
      if isLast
        then do
          putStrLn $ prefix ++ "└── " ++ show path
        else do
          putStrLn $ prefix ++ "├── " ++ show path
      if isDir
        then do
          let fullPath = currentPath </> path
          paths <- listDirectory fullPath
          printEntry (prefix ++ "    ") fullPath
        else return ()

-- Function to move a file
moveFile :: FilePath -> FilePath -> IO ()
moveFile source targetDir = do
  let fileName = takeFileName source
  let target = targetDir </> fileName
  createDirectoryIfMissing True targetDir
  renamePath source target

moveFileMenu :: IO ()
moveFileMenu = do
  putStrLn "Enter the files that you wanto move"
  source <- getLine
  putStrLn "Enter the name of folder want to move"
  targetDir <- getLine
  moveFile source targetDir
  putStrLn $ "Move file : " ++ source ++ " Successfull"

renameMenu :: FilePath -> IO ()
renameMenu currentPath = do
  putStrLn "Enter old of name that you want to change"
  oldName <- getLine
  putStrLn "Enter new of name that you want to change"
  newName <- getLine
  renameFile oldName newName

createDirMenu :: IO ()
createDirMenu = do
  putStrLn "Enter Name of Newfolder"
  newFolderName <- getLine
  createDirectory newFolderName
  putStrLn $ "Create the folder : " ++ newFolderName ++ " Successfull"

changeDir :: FilePath -> IO ()
changeDir currentPath = do
  printFolderTree currentPath
  putStrLn "Enter New path folder name or b back to parrent Folder "
  newPathName <- getLine
  case newPathName of
    "b" -> do
      let parentDir = takeDirectory currentPath
      setCurrentDirectory parentDir
      printFolderTree parentDir
      menuQ2 parentDir
    name -> do
      let fullNewPath = currentPath </> name
      isExitDir <- doesDirectoryExist fullNewPath
      if isExitDir
        then do
          setCurrentDirectory fullNewPath
          printFolderTree fullNewPath
          menuQ2 fullNewPath
        else putStrLn "folder not exits" >> menuQ2 currentPath

deleteFile :: IO ()
deleteFile = do
  putStrLn "Enter the name of file or folder want to delete?"
  fileName <- getLine
  removeFile fileName
  putStrLn $ "remove : " ++ fileName ++ "successfull!"

deleteDir :: IO ()
deleteDir = do
  putStrLn "Enter the name of folder want to delete?"
  folderName <- getLine
  deleteDirectoryRecursive folderName
  where
    deleteDirectoryRecursive :: FilePath -> IO ()
    deleteDirectoryRecursive dirPath = do
      exists <- doesDirectoryExist dirPath
      if exists
        then do
          contents <- listDirectory dirPath
          mapM_
            (\entry -> do
               let fullPath = dirPath ++ "/" ++ entry
               isDir <- doesDirectoryExist fullPath
               if isDir
                 then deleteDirectoryRecursive fullPath -- Recursively delete subdirectory
                 else removeFile fullPath)
            contents -- Delete file
          removeDirectory dirPath -- Delete the empty directory
        else putStrLn $ "Directory does not exist: " ++ dirPath

menuQ2 :: FilePath -> IO ()
menuQ2 currentPath = do
  putStrLn "\n1.Create newFolder"
  putStrLn "2.Move file to newFolder"
  putStrLn "3.Rename file"
  putStrLn "4.delete file"
  putStrLn "5.delete Folder"
  putStrLn "6.Print all Tree of folder"
  putStrLn "7.Change current Dir"
  putStrLn "8.Exit"
  putStrLn "Enter your option?"
  option <- getLine
  case option of
    "1" -> createDirMenu >> printFolderTree currentPath >> menuQ2 currentPath
    "2" -> moveFileMenu >> printFolderTree currentPath >> menuQ2 currentPath
    "3" ->
      renameMenu currentPath
        >> printFolderTree currentPath
        >> menuQ2 currentPath
    "6" -> printFolderTree currentPath >> menuQ2 currentPath
    "7" -> changeDir currentPath
    "4" -> deleteFile >> printFolderTree currentPath >> menuQ2 currentPath
    "5" -> deleteDir >> menuQ2 currentPath
    "8" -> putStrLn "Exiting..." >> return ()
    _ -> putStrLn "Invalid option , try again!" >> menuQ2 currentPath

mainMenuQ2 :: IO ()
mainMenuQ2 = do
  currentPath <- getCurrentDirectory
  menuQ2 currentPath

{- Question 3
Khi dùng cardano-cli để query các UTxO trên 1 địa chỉ, chúng ta nhận được kết quả dạng như sau:

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1f845ad67df909fcea522c7ff0d3c73ff17ada2436c1d65314acfd532e9789f0     0        9826876 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     0        33000000 lovelace + TxOutDatumNone
ac6f50aeb4427bc1f381163f0fec6f67d7542150a96904a54d274fd685b90f42     1        66177669 lovelace + TxOutDatumNone
d88ba55e07f9c8a091b87f2550aa97b331f05c0618bbbf8b43cf186ea38de3b8     0        4829226 lovelace + TxOutDatumNone
dd30de76b03db635eae4de221678a5190866f32f692598c87cefb8401e8ae7fb     0        4829226 lovelace + TxOutDatumNone

Các thông tin này đang được lưu trong file ./utxo.txt

Hãy viết hàm `selectUtxo` thực hiện các việc sau:
- Đọc nội dung file utxo.txt, bỏ 2 dòng đầu, in ra các dòng còn lại kèm theo số thứ tự của chúng tính từ 1.
- Yêu cầu người dùng lựa chọn 1 utxo theo số thứ tự.
    Kiểm tra tính hợp lệ của số nhập vào (nằm trong khoảng hợp lệ dựa trên số lượng utxo)
- In ra thông tin của utxo được chọn bao gồm: mã tham chiếu (có dạng <TxHash>#<TxIx>) và số lovelace chứa trong nó

Để rõ hơn, Hãy xem VD:

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
utxoFile = "utxo.txt"

loadFile :: FilePath -> IO [(Integer, String)]
loadFile file = do
  contents <- readFile file
  let indexedContents = zip [1 ..] (drop 3 $ lines contents)
  return indexedContents

outPutResult :: [String] -> String
outPutResult utxoItemList =
  "You select utxo "
    ++ head utxoItemList
    ++ "#"
    ++ utxoItemList !! 1
    ++ " with "
    ++ utxoItemList !! 2
    ++ " lovelace."

--Cach 1
-- selectUtxo :: FilePath -> IO ()
-- selectUtxo file = do
--   filteredContents <- loadFile file
--   putStrLn "Please enter the line number of utxo you want to select:"
--   option <- getLine
--   if any (`elem` ['a' .. 'z'] ++ ['A' .. 'Z']) option
--     then putStrLn "Invalid input. Please enter a number:"
--     else case lookup (read option :: Integer) filteredContents of
--            Nothing ->
--              putStrLn
--                "Invalid number. Please enter a line number in valid range:"
--            Just utxo -> do
--              putStrLn $ outPutResult $ words utxo
--Cach 2
selectUtxo :: FilePath -> IO ()
selectUtxo file = do
  filteredContents <- loadFile file
  putStrLn "Please enter the line number of the utxo you want to select:"
  option <- getLine
  case readMaybe option of
    Nothing -> putStrLn "Invalid input. Please enter a number."
    Just lineNumber ->
      case lookup lineNumber filteredContents of
        Nothing -> putStrLn "Invalid number. Please enter a valid line number."
        Just utxo -> putStrLn $ outPutResult $ words utxo

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
-- cach 1
sumNumbers :: IO ()
sumNumbers = do
  putStrLn "Enter numbers (one per line), and an empty line to end:"
  total <- sumNumbers' 0
  putStrLn $ "The sum is: " ++ show total

sumNumbers' :: Int -> IO Int
sumNumbers' acc = do
  input <- getLine
  if null input
    then return acc
    else do
      case readMaybe input of
        Just num -> sumNumbers' (acc + num)
        Nothing  -> sumNumbers' acc
--cach 2
-- inputValues :: IO [String]
-- inputValues = do
--   putStrLn "Enter numbers (one per line), and an empty line to end:"
--   value <- getLine
--   if value == ""
--     then return []
--     else do
--       colection <- inputValues
--       return (value : colection)
-- isInteger :: String -> Bool
-- isInteger s =
--   case reads s :: [(Integer, String)] of
--     [(n, "")] -> True
--     _         -> False
-- sumNumbers :: IO ()
-- sumNumbers = do
--   values <- inputValues
--   let result =
--         foldl'
--           (\acc v ->
--              if isInteger v
--                then acc + read v :: Integer
--                else acc)
--           0
--           values
--   putStrLn $ "The sum is: " ++ show result
