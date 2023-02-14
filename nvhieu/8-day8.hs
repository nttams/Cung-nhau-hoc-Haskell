-- ===================================================================
-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".

-- Câu hỏi 1
-- Các loại rượu khác nhau được làm từ các loại nho khác nhau, có khoảng 10000 loại trên thế giới!
-- Tạo một loại từ đồng nghĩa gọi là "Grape" cho các tên nho khác nhau dưới dạng chuỗi.
-- Ngoài ra, hãy sử dụng loại từ đồng nghĩa này cho các loại nho: "Sangiovese", "Cabernet-sauvignon", "Merlot" và "Garnacha".

type Grape = String

data GrapeName = GrapeName { grape :: Grape } deriving(Show)

sangiovese = GrapeName {grape = "Sangiovese"}

cabernetSauvignon = GrapeName {grape = "Cabernet-sauvignon"}

merlot = GrapeName {grape = "Merlot"}

garnacha = GrapeName {grape = "Garnacha"}


tellGrape :: GrapeName -> String

tellGrape (GrapeName { grape = g}) = "The grapes of " ++ g ++ " are known to make delicious wine!"

-- tellGrape sangiovese
   
-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.

-- Câu hỏi 2
-- Các khu vực xuất khẩu rượu vang nổi tiếng nhất nằm ở Pháp, Ý và Tây Ban Nha.
-- Mỗi quốc gia này được chia thành các khu vực nhỏ hơn.
-- Những vùng nhỏ hơn này được biết đến với một phong cách nhất định, ví dụ như vùng Champagne ở Pháp
-- Tạo một loại từ đồng nghĩa có tên là "Region" cho vùng rượu vang dựa trên quốc gia và vùng của họ dưới dạng một bộ chuỗi.
-- Ngoài ra, hãy sử dụng loại từ đồng nghĩa này cho các vùng: Bordeaux ở Pháp, Tuscany ở Ý và Rioja ở Tây Ban Nha.

type Country = String
type Province =String

type Region = (Country,Province)

data WineRegion = WineRegion { region:: Region} deriving(Show)


bordeaux = WineRegion { region = ("Bordeaux", "France")}

tuscany = WineRegion { region = ("Tuscany", "Italy")}

rioja = WineRegion { region = ("Rioja", "Spain")}

tellWineRegion :: WineRegion -> String
tellWineRegion (WineRegion { region = r}) = "The region of " ++ fst r ++ ", " ++ snd r ++ " makes exquisite wine!"


-- tellWineRegion bordeaux


-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.

-- Câu 3
-- Rượu vang có ba loại, đó là rượu vang đỏ, trắng hoặc hồng.
-- Ngoài loại ra, mỗi loại rượu còn có một nồng độ cồn nhất định.
-- Tạo một Kiểu dữ liệu có tên là "Loại" đại diện cho ba loại này, với mỗi loại ghi lại mức độ cồn.
-- Ngoài ra, hãy sử dụng loại "data" này cho các ví dụ: rượu vang đỏ có nồng độ cồn 14,5%, rượu vang trắng có nồng độ cồn 13%
-- và rượu vang hồng với 12% cồn.


type Kinds = String 
type Level =Float

type Kind = (Kinds, Level)

data WineKind = WineKind { kind :: Kind} deriving(Show)


red = WineKind{ kind = ("Red", 14.5)}

white = WineKind{ kind = ("White", 13)}

rose = WineKind{ kind = ("Rose", 12)}  



tellWineKind :: WineKind -> String
tellWineKind (WineKind { kind = k}) = "This kind of wine is " ++ fst k ++ " with " ++ show(snd k) ++ "% alcohol!"

-- tellWineKind rose

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a wine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.

-- Câu 4
-- Trong thế giới rượu vang, chai rượu hiển thị tất cả các thông tin trên cho người tiêu dùng trên nhãn của nó.
-- Tạo một loại bản ghi có tên là "Label" ghi lại những quả nho đang ở trạng thái wine, vùng xuất phát của nó, và nó loại rươụ. Lưu ý rằng một số loại rượu vang là sự kết hợp pha trộn của nhiều loại nho!
-- Ngoài ra, hãy tạo cho mỗi loại rượu được mô tả bên dưới một nhãn.

-- Larrosa Rose là một loại rượu vang hồng từ vùng Rioja. Nó được làm từ nho Garnacha và
-- có nồng độ cồn 14%.

-- Castiglioni là một loại rượu vang đỏ từ vùng Tuscany. Nó được làm từ nho Sangaguese và
-- có nồng độ cồn 12,5%.

-- Bordeaux được biết đến với rượu vang đỏ, chủ yếu là sự pha trộn giữa Cabernet-sauvignon và Merlot.
-- Tạo Nhãn cho rượu "Le Petit Haut Lafitte" có nồng độ cồn 13,5%.

data Label = Label {nameLabel:: String

                    , grapeLabel::Grape

                    , regionLabel::Region

                    , kindLabel::Kind} deriving(Show)



-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and has a alcohol level of 14%.

larrosaRose = Label {nameLabel="Larrosa Rose", grapeLabel = "Garnacha", regionLabel = ("Rioja", "Spain"), kindLabel = ("Rose", 14)}

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%

castiglioni = Label {nameLabel = "Castiglioni", grapeLabel = "Sangiovese", regionLabel = ("Tuscany", "Italy"), kindLabel = ("Red", 12.5)}

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

lePetitHautLafitte = Label {nameLabel = "Le Petit Haut Lafitte", grapeLabel = "Cabernet-Sauvignon and Merlot", regionLabel = ("Bordeux", "France"), kindLabel = ("Red", 13.5)}


tellLabel :: Label -> String
tellLabel (Label {nameLabel = n

                    , grapeLabel = g

                    , regionLabel = r

                    , kindLabel = k})

    = "This wine is called " ++ n ++ " and it is made with the finest " ++ g ++ " grapes in the region of " ++ (fst r) ++ ", " ++ (snd r) ++ "! It is a " ++ fst k ++ " kind of wine with " ++ show(snd k) ++ "% alcohol."


-- tellLabel larrosaRose
 
-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
-- This is a test list for the `containsGrape` function with an grape that is not in the list.


-- Câu 5
-- Viết hàm `containsGrape` lấy danh sách Nhãn và Nho và trả về một giá trị logic.
-- Hàm sẽ kiểm tra xem có tồn tại rượu trong Nhãn chứa Nho này không.

-- Đây là danh sách kiểm tra chức năng `containsGrape` với một loại nho không có trong danh sách.
-- GrapeList = [larrosaRose,castiglioni,lePetitHaitLafitte]
-- newGrape = "Pinot Noir"


grapeList = [larrosaRose,castiglioni,lePetitHautLafitte]

newGrape = "Pinot Noir"

containsGrape :: [Label] -> String -> [Label]

containsGrape grapeList name = filter (\x -> (nameLabel x) == name) grapeList

-- containsGrape grapeList newGrape








