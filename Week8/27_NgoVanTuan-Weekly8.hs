-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".

-- Các loại rượu vang khác nhau được làm từ các loại nho khác nhau, có khoảng 10000 loại nho trên thế giới!
-- Tạo một kiểu đồng nghĩa với String, tên là "Grape", để biểu diễn tên các loại nho.
-- Sử dụng kiểu mới tạo để khai báo các loại nho: "Sangiovese", "Cabernet-sauvignon", "Merlot" và "Garnacha".

type Grape = String

sangiovese :: Grape
sangiovese = "Sangiovese"

cabernetSauvignon :: Grape
cabernetSauvignon = "Cabernet-sauvignon"

merlot :: Grape
merlot = "Merlot"

garnacha :: Grape
garnacha = "Garnacha"

-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.

-- Các vùng xuất khẩu rượu vang nổi tiếng nhất nằm ở Pháp, Ý và Tây Ban Nha.
-- Mỗi quốc gia này được chia thành các vùng nhỏ hơn.
-- Các vùng nhỏ hơn này nổi tiếng với một phong cách nhất định, ví dụ như vùng Champagne ở Pháp.
-- Tạo một kiểu đồng nghĩa với (String, String), có tên là "Region", để biểu diễn vùng rượu vang, gồm tên quốc gia và tên vùng
-- Sử dụng kiểu này biểu diễn các vùng: Bordeaux ở Pháp, Tuscany ở Ý, và Rioja ở Tây Ban Nha.

type Region = (String, String)

bordeaux :: Region
bordeaux = ("France", "Bordeaux")

tuscany :: Region
tuscany = ("Italy", "Tuscany")

rioja :: Region
rioja = ("Spain", "Rioja")

-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.

-- Rượu vang có 3 ba loại: rượu vang đỏ, rượu vang trắng hoặc rượu vang hồng.
-- Ngoài ra, mỗi chai rượu vang cũng có một nồng độ cồn nhất định.
-- Tạo một kiểu dữ liệu có tên là "Kind" đại diện cho ba loại rượu vang và cho biết cả độ cồn.
-- Sử dụng kiểu dữ liệu này cho các ví dụ: rượu vang đỏ với 14.5% cồn, rượu vang trắng với 13% cồn
-- và rượu vang hồng với 12% cồn.

data WineType = RedWine | WhiteWine | RoseWine deriving (Show)
type AlcoholConcentration = Double 
type Kind = (WineType, AlcoholConcentration)

example1 :: Kind
example1 = (RedWine, 14.5)  

example2 :: Kind
example2 = (WhiteWine, 13.0)  

example3 :: Kind
example3 = (RoseWine, 12.0)  

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and 
-- has a alcohol level of 14%.

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

-- Trong thế giới rượu vang, các chai rượu hiển thị tất cả thông tin trên nhãn để người tiêu dùng biết.
-- Tạo một kiểu record có tên là "Label" để biểu diễn: các loại nho có trong rượu vang, vùng sản xuất của nó,
-- và loại của nó. Lưu ý rằng một số loại rượu vang là sự kết hợp pha trộn của nhiều loại nho!
-- Sau đó, tạo một nhãn cho mỗi loại rượu vang được mô tả dưới đây.

-- Larrosa Rose là rượu vang hồng từ vùng Rioja. Nó được làm từ nho Garnacha và
-- có độ cồn là 14%.
--larrosaRose = _todo

-- Castiglioni là rượu vang đỏ từ vùng Tuscany. Nó được làm từ nho Sangiovese và
-- có mức độ cồn là 12.5%.
--castiglioni = _todo

-- Bordeaux nổi tiếng với rượu vang đỏ, chúng là sự pha trộn giữa Cabernet-sauvignon và Merlot.
-- Tạo một nhãn cho rượu vang "Le Petit Haut Lafitte" có mức độ cồn là 13.5%.
--lePetitHaitLafitte = _todo

data Label = Label {
  grapes :: [Grape],
  region :: Region,
  kind :: Kind
} deriving (Show)

larrosaRose :: Label
larrosaRose = Label {
  grapes = [garnacha],
  region = rioja,
  kind = (RoseWine, 14.0)
}

castiglioni :: Label
castiglioni = Label {
  grapes = [sangiovese],
  region = tuscany,
  kind = (RedWine, 12.5)
}

lePetitHautLafitte :: Label
lePetitHautLafitte = Label {
  grapes = [cabernetSauvignon, merlot],
  region = bordeaux,
  kind = (RedWine, 13.5)
}

-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
-- Viết hàm `containsGrape` nhận vào một danh sách các nhãn và một loại nho, rồi trả về một giá trị boolean.
-- Hàm này kiểm tra xem có tồn tại loại rượu vang nào trong nhãn chứa loại nho này hay không.

-- This is a test list for the `containsGrape` function with an grape that is not in the list.
grapeList :: [Label]
grapeList = [larrosaRose, castiglioni, lePetitHautLafitte]

newGrape :: Grape
newGrape = "Pinot Noir"

containsGrape :: [Label] -> Grape -> Bool
containsGrape labels grape = any (\label -> grape `elem` grapes label) labels
