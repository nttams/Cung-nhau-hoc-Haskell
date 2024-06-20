{-
Inventory Management System: 
Define a product type with various properties (id, name, price, quantity) 
and create operations for searching, adding products and updating a quantity of a product.

Hệ thống quản lý kho:
Định nghĩa những types như Inventory, ProductInfo với một số thuộc tính như id, name, price và quantity
Viết hàm tìm kiếm sản phẩm theo id, thêm 1 sản phẩn và cập nhật số lượng của 1 sản phẩm.
-}

data Product = Product {
  productId :: Int,
  productName :: String,
  price :: Float,
  quantity :: Int
} deriving (Show)

-- search producs by ids
searchProductsByIds :: [Product] -> [Int] -> [Product]
searchProductsByIds products (pId:pIds) = 
    filter (\p -> pId == getId p ) products ++ searchProductsByIds products pIds
searchProductsByIds products [] = []

-- add a new product
addProduct :: [Product] -> Product -> [Product]
addProduct products newProduct 
    | getId newProduct `elem` getIDList products = products
    | otherwise = newProduct : products 

getId :: Product -> Int
getId (Product pId n p qty) = pId

getIDList :: [Product] -> [Int]
getIDList (p:ps) = getId p : getIDList ps
getIDList [] = []

-- update a quantity of the product
updateQuantity :: [Product] -> Int -> Int -> [Product]
updateQuantity products pId newQuantity = map (updateProduct pId newQuantity) products

updateProduct :: Int -> Int -> Product -> Product
updateProduct id newQuantity products 
    | getId products == id =  products { quantity = newQuantity }
    | otherwise = products

-- Testing phase
-- Sample products
products = [Product 1 "iPhone 15" 1000.00 20, 
            Product 2 "iPhone 15 Pro" 1200 15, 
            Product 3 "iPhone 15 Pro Max" 1500 6]

-- Search for products by IDs
searchResults = searchProductsByIds products [1,3]

-- Add a new product
newProduct = Product 4 "Trousers" 15.0 35
addedProducts = addProduct products newProduct

-- Update quantity of product with ID 2
updatedQuantity = updateQuantity addedProducts 2 10


-- ________________________
-- data ProductInfo = ProductInfo { id :: ID, name :: String, price :: Float} deriving (Show)

-- type ID = Int

-- data Quantity a = Quantity a deriving (Show)

-- data Inventory = Empty | Inventory ProductInfo (Quantity Int) Inventory deriving (Show)

-- inventoryTestData :: Inventory
-- inventoryTestData = Inventory (ProductInfo 1 "iPhone 15" 1000.00) (Quantity 100)
--                                 (Inventory (ProductInfo 2 "iPhone 15 Pro" 1300.00) (Quantity 500)
--                                     ( Inventory (ProductInfo 3 "iPhone 15 Pro Max" 1500.00) (Quantity 200)
--                                      Empty
--                                     )
--                                 )

-- getProductInfo :: Inventory -> [ProductInfo]
-- getProductInfo (Inventory pInfo qty inventory) = pInfo : getProductInfo inventory
-- getProductInfo _ = []

-- getID :: ProductInfo -> ID
-- getID (ProductInfo id name price) = id

-- searchProductByID :: Inventory -> [ID] -> [ProductInfo]
-- searchProductByID inventory [] = []
-- searchProductByID inventory (id:ids) = (filter (\pID -> id == getID pID) $ getProductInfo inventory) ++ searchProductByID inventory ids

-- checkProductQuantityByID :: Inventory -> ID -> Maybe (Quantity Int) 
-- checkProductQuantityByID Empty _ = Nothing -- Quantity 0
-- checkProductQuantityByID (Inventory pInfo qty inventory) id 
--     | id == getID pInfo = Just $ qty
--     | otherwise = checkProductQuantityByID inventory id

-- updateProductQuantityByID :: Inventory -> Quantity Int -> ID -> Inventory
-- updateProductQuantityByID Empty _ _ = Empty
-- updateProductQuantityByID (Inventory pInfo qty inventory) newQ id
--     | id == getID pInfo = Inventory pInfo newQ inventory
--     | otherwise = updateProductQuantityByID inventory newQ id