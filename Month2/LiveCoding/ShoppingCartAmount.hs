{-}
Assumption: there is an ecommerce site
write a function to filter products by brands or categories
and a function - calculate total prices of the products in Cart

Giả định cho một site thương mại điện tử:
viết hàm filter sản phẩm bởi brands/categories
và tính tổng số tiền có trong giỏ hàng.
-}
allProducts = [("Laptop", 500.00, "electronics"),
                ("Keyboard", 20.00, "accessories"),
                ("Mouse", 20.00, "accessories"),
                ("Headphones", 100.00, "electronics"),
                ("Iphone 15", 1000.00, "Apple"),
                ("Iphone 14", 600.00, "Apple"),
                ("SS A54", 200.00, "SamSung")]

-- filter products by brands/categories
filterProductByCategory :: [String] -> [(String, Double, String)]
filterProductByCategory [] = []
filterProductByCategory (category:categories) = 
    filter ((==) category . getCategory) allProducts ++ filterProductByCategory categories

getCategory :: (String, Double, String) -> String
getCategory (name, price, category) = category

-- assumption: add products to Cart
totalPriceInCart :: [(String, Double, String)] -> Double
-- totalPriceInCart [] = 0
-- totalPriceInCart (product:products)= getPrice product + totalPriceInCart products
totalPriceInCart = foldr ((+) . getPrice) 0

getPrice :: (String, Double, String) -> Double
getPrice (name, price, category) = price

-- filter products by name
filterProductByName :: [String] -> [(String, Double, String)]
filterProductByName [] = []
filterProductByName (name:names) =
    filter ((==) name . getName) allProducts ++ filterProductByName names

getName :: (String, Double, String) -> String
getName (name, price, category) = name