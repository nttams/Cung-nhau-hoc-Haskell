-- 👉 Tiêu đề: Chương Trình Quản lý Thư Viện Bằng Haskell
-- 👉 Nội dung:
--           -Chương trình cho phép quản lý sách và thành viên.Hỗ trợ các chức năng mượn sách, trả sách, hiển thị thông tin sách và thành viên, ghi lại lịch sử mượn trả của thành viên và của từng loại sách...
-- 👉 Chức năng:
--     1. Mượn sách: Người dùng có thể mượn sách bằng cách nhập mã
--     2. Trả sách: Người dùng có thể trả sách bằng cách nhập mã ID thành viên và ID sách. Hệ thống sẽ kiểm tra danh sách sách đã mượn và cập nhật trạng thái mượn trả.
--     3. Hiển thị trạng thái sách: Hiển thị danh sách sách trong hệ thống cùng với số lượng còn lại.
--     4. Hiển thị trạng thái mượn của thành viên: Hiển thị danh sách các thành viên cùng với các sách họ đã mượn.
--     5. Hiển thị lịch sử mượn trả: Hiển thị lịch sử mượn trả của một sách hoặc thành viên cụ thể.
--     6. Lưu Lại Database : Lưu thành file dữ liệu các thành viên, các loại sách, số lượng tổng và tồn kho,lịch sử mượn , trả cho từng loại sách và từng thành viên thư viện ….
--     7. Thoát: Thoát khỏi chương trình.
-- 👉 Các kiến thức Haskell sử dụng trong chương trình : Data types, Pattern matching and Case expressions,Recursion and Folds
-- ,Hyder order function,Parameter Types,Creating Type Classes and Instances,Basic I/O....
import           Data.List        (delete, find, sort)
import           System.Directory (removeFile, renameFile)
import           System.IO

type Hist = (String, Int, String) -- (ID, Quantity, Date)

type BorrowStatus = (String, Int)

-- Data Definitions
data Book = Book
  { bookID, bookTitle, bookAuthor        :: String
  , bookQuantities                       :: Int
  , bookBorrowHistory, bookReturnHistory :: [Hist]
  } deriving (Show, Read, Eq, Ord)

data Member = Member
  { memberId, memberName                     :: String
  , borrowStatus                             :: [BorrowStatus] -- (BookID, Quantity)
  , memberBorrowHistory, memberReturnHistory :: [Hist]
  } deriving (Show, Read, Eq, Ord)

data Transaction
  = Borrow
      { borrowBookID, borrowMemberID, borrowDate :: String
      }
  | Return
      { returnBookID, returnMemberID, returnDate :: String
      }
  deriving (Show, Read, Eq, Ord)

type Database = ([Book], [Member])

type Data = (Book, Member)

class LibraryItem a where
  getID :: a -> String
  getHistory :: a -> ([Hist], [Hist])
  display :: a -> String
  displayAllStatus :: [a] -> IO ()

instance LibraryItem Member where
  getID = memberId
  getHistory m = (memberBorrowHistory m, memberReturnHistory m)
  display m =
    "Member ID: "
      ++ memberId m
      ++ concatMap
           (\(bid, qty) ->
              "\n    Book ID : "
                ++ bid
                ++ "   Quantities borrowed : "
                ++ show qty)
           (borrowStatus m)
  displayAllStatus = mapM_ (putStrLn . display) . sort

instance LibraryItem Book where
  getID = bookID
  getHistory b = (bookBorrowHistory b, bookReturnHistory b)
  display b =
    "Book ID: "
      ++ bookID b
      ++ "\n  Remaining Quantities: "
      ++ show (bookQuantities b)
  displayAllStatus = mapM_ (putStrLn . display) . sort

-- Helper functions
--prompt: Prompts the user for input with a message.
prompt :: String -> IO String
prompt text = putStrLn text >> getLine

--findItem: Finds an member or book in a list of members or books by ID
findItem :: Eq b => (a -> b) -> b -> [a] -> Maybe a
findItem f x = find ((== x) . f)

--loadFiles: Loads data from a file and reads it as a list.
loadFiles :: Read a => FilePath -> IO [a]
loadFiles = fmap read . readFile

--saveFiles: Saves a list of data to a file.
saveFiles :: Show a => FilePath -> [a] -> IO ()
saveFiles filePath values = do
  (tempFile, tempHandle) <- openTempFile "." filePath
  hClose tempHandle
  writeFile tempFile (show values)
  removeFile filePath
  renameFile tempFile filePath

-- Borrow and Return book functions
--borrowBook: Handles the borrowing of a book by a member.
borrowBook :: Data -> Database -> Int -> String -> (Database, Maybe Transaction)
borrowBook (book, member) db quantity date
  | bookQuantities book >= quantity =
    let newBook =
          book
            { bookQuantities = bookQuantities book - quantity
            , bookBorrowHistory =
                (memberId member, quantity, date) : bookBorrowHistory book
            }
        newMember =
          member
            { borrowStatus =
                case lookup (bookID book) (borrowStatus member) of
                  Just oldBorrowstatus ->
                    updateStatus (bookID book) quantity (borrowStatus member)
                  Nothing -> (bookID book, quantity) : borrowStatus member
            , memberBorrowHistory =
                (bookID book, quantity, date) : memberBorrowHistory member
            }
        transaction = Borrow (bookID book) (memberId member) date
     in ( (newBook : delete book (fst db), newMember : delete member (snd db))
        , Just transaction)
  | otherwise = (db, Nothing)

--updateStatus: Updates the borrow status of a member.
updateStatus :: String -> Int -> [BorrowStatus] -> [BorrowStatus]
updateStatus bookID quantity =
  filter ((/= 0) . snd)
    . map
        (\(bid, qty) ->
           if bid == bookID
             then (bid, qty + quantity)
             else (bid, qty))

--returnBook: Handles the returning of a book by a member.
returnBook :: Data -> Database -> Int -> String -> (Database, Maybe Transaction)
returnBook (book, member) db quantity date
  | Just borrowedQuantity <- lookup (bookID book) (borrowStatus member)
  , borrowedQuantity >= quantity =
    let newBook =
          book
            { bookQuantities = bookQuantities book + quantity
            , bookReturnHistory =
                (memberId member, quantity, date) : bookReturnHistory book
            }
        newMember =
          member
            { borrowStatus =
                updateStatus (bookID book) (-quantity) (borrowStatus member)
            , memberReturnHistory =
                (bookID book, quantity, date) : memberReturnHistory member
            }
        transaction = Return (bookID book) (memberId member) date
     in ( (newBook : delete book (fst db), newMember : delete member (snd db))
        , Just transaction)
  | otherwise = (db, Nothing)

-- Main menu
--main: The entry point of the program.
main :: IO ()
main = do
  books <- loadFiles "books.txt"
  members <- loadFiles "members.txt"
  putStrLn "Library Management System"
  mainMenu (books, members)

--mainMenu: The main menu of the program.
mainMenu :: Database -> IO ()
mainMenu db = do
  putStrLn "\n1. Borrow a Book"
  putStrLn "2. Return a Book"
  putStrLn "3. Display Status of Books"
  putStrLn "4. Display Borrowed Status of Members"
  putStrLn "5. Display History of Book/Member by ID"
  putStrLn "6. Save"
  putStrLn "7. Exit"
  option <- prompt "Choose an option: "
  case option of
    "1" -> borrowBookMenu db
    "2" -> returnBookMenu db
    "3" -> displayAllStatus (fst db) >> mainMenu db
    "4" -> displayAllStatus (snd db) >> mainMenu db
    "5" -> displayHistoryMenu db >> mainMenu db
    "6" -> saveData db >> mainMenu db
    "7" -> exit
    _   -> putStrLn "Invalid option, try again!" >> mainMenu db

-- Sub menus
--borrowBookMenu: Handles the borrow book menu.
borrowBookMenu :: Database -> IO ()
borrowBookMenu db@(books, members) = do
  mid <- prompt "Who wants to borrow a book? (Member ID): "
  case findItem memberId mid members of
    Nothing -> putStrLn "Invalid Member ID" >> mainMenu db
    Just member -> do
      putStrLn "\nAvailable Books:"
      mapM_ (putStrLn . display) (filter ((> 0) . bookQuantities) books)
      bid <- prompt "Enter the Book ID to borrow: "
      case findItem bookID bid books of
        Nothing -> putStrLn "Invalid Book ID" >> mainMenu db
        Just book -> do
          quantityStr <- prompt "Enter the quantity to borrow: "
          let quantity = read quantityStr :: Int
          date <- prompt "Enter the date of transaction (DD-MM-YYYY): "
          let (newDB, newTransaction) =
                borrowBook (book, member) db quantity date
          case newTransaction of
            Just _ -> confirmTransaction "borrow" newDB db
            Nothing ->
              putStrLn "Failed to borrow book - not enough copies available"
                >> mainMenu db

--confirmTransaction: Confirms a transaction.
confirmTransaction :: String -> Database -> Database -> IO ()
confirmTransaction action newDB db = do
  confirm <-
    prompt $ "Are you sure you want to " ++ action ++ " this book? (yes/no): "
  if confirm == "yes"
    then putStrLn ("Book " ++ action ++ "ed successfully!") >> mainMenu newDB
    else mainMenu db

--returnBookMenu: Handles the return book menu.
returnBookMenu :: Database -> IO ()
returnBookMenu db@(books, members) = do
  mid <- prompt "Who wants to return a book? (Member ID): "
  case findItem memberId mid members of
    Nothing -> putStrLn "Invalid Member ID" >> mainMenu db
    Just member -> do
      putStrLn "\nBorrowed Books:"
      mapM_
        (\(bid, qty) ->
           putStrLn
             $ "Borrowed book ID : "
                 ++ bid
                 ++ "\n Borrowed quantities : "
                 ++ show qty)
        (borrowStatus member)
      bid <- prompt "Enter the Book ID to return: "
      case findItem bookID bid books of
        Nothing -> putStrLn "Invalid Book ID" >> mainMenu db
        Just book -> do
          quantityStr <- prompt "Enter the quantity to return: "
          let quantity = read quantityStr :: Int
          date <- prompt "Enter the date of transaction (DD-MM-YYYY): "
          let (newDB, newTransaction) =
                returnBook (book, member) db quantity date
          case newTransaction of
            Just _  -> confirmTransaction "return" newDB db
            Nothing -> putStrLn "Failed to return book" >> mainMenu db

-- Display history
--displayHistory: Displays the history of an item.
displayHistory :: LibraryItem a => a -> IO ()
displayHistory item = do
  let (borrows, returns) = getHistory item
  putStrLn $ "\nBorrow History of ID " ++ getID item ++ ":"
  mapM_
    (\(id, quantity, date) ->
       putStrLn
         $ " ID: "
             ++ id
             ++ "\n   Quantity: "
             ++ show quantity
             ++ "\n   Borrowed on: "
             ++ date)
    borrows
  putStrLn $ "\nReturn History of ID " ++ getID item ++ ":"
  mapM_
    (\(id, quantity, date) ->
       putStrLn
         $ " ID: "
             ++ id
             ++ "\n   Quantity: "
             ++ show quantity
             ++ "\n   Returned on: "
             ++ date)
    returns

--displayHistoryMenu: Displays the history menu.
displayHistoryMenu :: Database -> IO ()
displayHistoryMenu (books, members) = do
  id <- prompt "Enter the Book or Member ID: "
  case (findItem bookID id books, findItem memberId id members) of
    (Just book, _)     -> displayHistory book
    (_, Just member)   -> displayHistory member
    (Nothing, Nothing) -> putStrLn "Invalid Book or Member ID"

--saveData: Saves the database to files.
saveData :: Database -> IO ()
saveData (books, members) = do
  saveFiles "books.txt" books
  saveFiles "members.txt" members
  putStrLn "Data saved successfully!"

--exit: Exits the program.
exit :: IO ()
exit = putStrLn "Exiting..." >> return ()
