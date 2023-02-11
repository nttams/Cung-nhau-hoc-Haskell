-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".

type Grape = String

data GrapeName = GrapeName { grape :: Grape } deriving(Show)

tellGrape :: GrapeName -> String

tellGrape (GrapeName { grape = g}) = "The grapes of " ++ g ++ " are known to make delicious wine!"

sangiovese = GrapeName {grape = "Sangiovese"}

cabernetSauvignon = GrapeName {grape = "Cabernet-sauvignon"}

merlot = GrapeName {grape = "Merlot"}

garnacha = GrapeName {grape = "Garnacha"}

-- tellGrape sangiovese
   
-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.

type Region = (String,String)

data WineRegion = WineRegion { region:: Region} deriving(Show)

tellWineRegion :: WineRegion -> String

tellWineRegion (WineRegion { region = r}) = "The region of " ++ fst r ++ ", " ++ snd r ++ " makes exquisite wine!"

bordeaux = WineRegion { region = ("Bordeaux", "France")}

tuscany = WineRegion { region = ("Tuscany", "Italy")}

rioja = WineRegion { region = ("Rioja", "Spain")}

-- tellWineRegion bordeaux
-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.

type Kind = (String, Float)

data WineKind = WineKind { kind :: Kind} deriving(Show)

tellWineKind :: WineKind -> String

tellWineKind (WineKind { kind = k}) = "This kind of wine is " ++ fst k ++ " with " ++ show(snd k) ++ "% alcohol!"

red = WineKind{ kind = ("Red", 14.5)}

white = WineKind{ kind = ("White", 13)}

rose = WineKind{ kind = ("Rose", 12)}  

-- tellWineKind rose
-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a wine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.

data Label = Label {nameLabel:: String

                    , grapeLabel::Grape

                    , regionLabel::Region

                    , kindLabel::Kind} deriving(Show)

tellLabel :: Label -> String

tellLabel (Label {nameLabel = n

                    , grapeLabel = g

                    , regionLabel = r

                    , kindLabel = k})

    = "This wine is called " ++ n ++ " and it is made with the finest " ++ g ++ " grapes in the region of " ++ (fst r) ++ ", " ++ (snd r) ++ "! It is a " ++ fst k ++ " kind of wine with " ++ show(snd k) ++ "% alcohol."

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and 
-- has a alcohol level of 14%.

larrosaRose = Label {nameLabel="Larrosa Rose", grapeLabel = "Garnacha", regionLabel = ("Rioja", "Spain"), kindLabel = ("Rose", 14)}

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%

castiglioni = Label {nameLabel = "Castiglioni", grapeLabel = "Sangiovese", regionLabel = ("Tuscany", "Italy"), kindLabel = ("Red", 12.5)}

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

lePetitHautLafitte = Label {nameLabel = "Le Petit Haut Lafitte", grapeLabel = "Cabernet-Sauvignon and Merlot", regionLabel = ("Bordeux", "France"), kindLabel = ("Red", 13.5)}
-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
-- This is a test list for the `containsGrape` function with an grape that is not in the list.

grapeList = [larrosaRose,castiglioni,lePetitHautLafitte]

newGrape = "Pinot Noir"

containsGrape :: [Label] -> String -> [Label]

containsGrape grapeList name = filter (\x -> (nameLabel x) == name) grapeList
