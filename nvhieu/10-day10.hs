{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
-}

data Box a       = Empty          | Has a            deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a   deriving (Show)

class Container c where
    isEmpty  :: c a -> Bool
    contains :: (Eq a) => c a -> a -> Bool
    replace  :: c a -> b -> c b
    unwrap   :: c a -> a
    
  

instance Container Box where 
    isEmpty Empty = True
    isEmpty _     = False

    contains (Has x) y = x==y
    contains Empty _   = False

    replace _ x = Has x
  
    unwrap (Has x)  = x
  

instance Container (Present t) where
  
    isEmpty (EmptyPresent _) =True
    isEmpty _                = False

    contains (PresentFor _ x) y = x==y
    contains (EmptyPresent _) _ = False

    replace (PresentFor tag _) x = PresentFor tag x
    replace (EmptyPresent tag) x = PresentFor tag x

    unwrap (PresentFor _ x) = x




{-
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving (Show)

instance Container (MailedBox t d)  where
    
    isEmpty (EmptyMailBox _ _)  = True
    isEmpty _                   = False
    
    contains (MailBoxTo _ _ x) y = x == y
        
    replace (MailBoxTo tag y _) x = MailBoxTo tag y x
    

    unwrap (MailBoxTo _ _ x)  = x

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double deriving (Show, Eq)

instance Ord Salary where
  compare (USD x) (USD y) = compare x y
  compare (USD x) (EUR y) = compare x (y * 1.1)
  compare (EUR x) (USD y) = compare (x * 1.1) y
  compare (EUR x) (EUR y) = compare x y

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show, Eq , Ord)

nvhieu= Contractor Manager Managing (EUR 1000) "Tay ho"
nvxx = Employee Junior Programming (USD 500) "Tay ho"

-- write a haskell function that compares salary

compareSalary :: Relationship -> Relationship -> Ordering
compareSalary (Contractor _ _ sal1 _) (Employee _ _ sal2 _) = compare sal1 sal2
compareSalary (Employee _ _ sal1 _) (Contractor _ _ sal2 _) = compare sal1 sal2
compareSalary (Contractor _ _ sal1 _) (Contractor _ _ sal2 _) = compare sal1 sal2
compareSalary (Employee _ _ sal1 _) (Employee _ _ sal2 _) = compare sal1 sal2



-- write a haskell function that compares Position of nvhieu and nvxx
comparePositions :: Relationship -> Relationship -> Bool
comparePositions (Contractor pos1 _ _ _) (Employee pos2 _ _ _) = pos1 == pos2
comparePositions (Employee pos1 _ _ _) (Contractor pos2 _ _ _) = pos1 == pos2
comparePositions (Contractor pos1 _ _ _) (Contractor pos2 _ _ _) = pos1 == pos2
comparePositions (Employee pos1 _ _ _) (Employee pos2 _ _ _) = pos1 == pos2


data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }
  --deriving (Show, Eq, Ord)

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3



-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- -- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Eq)

{-
Prelude> :i Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
    -- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
-}

instance Num Exp where
  Exp x + Exp y = Exp (x + y)
  Exp x * Exp y = Exp (x * y)
  abs (Exp x) = Exp (abs x)
  signum (Exp x) = Exp (signum x)
  fromInteger n = Exp (fromInteger n)
  negate (Exp x) = Exp (negate x)
  
--
-- -- Team memeber data
type TeamMember = (String, Exp)
--
-- -- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]
--
-- -- Function to check the combined experience of the team
-- -- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
