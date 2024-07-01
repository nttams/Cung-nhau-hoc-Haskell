{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}
 
data Box a       = Empty          | Has a            deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a   deriving (Show)

class Container c  where
    isEmpty :: c a ->Bool
    containe :: Eq a=>c a ->a->Bool
    replace :: c a -> a -> c a
    unwrap :: c a -> Maybe a

instance Container Box  where
    isEmpty Empty = True
    isEmpty _ = False

    containe Empty _ = False
    containe (Has x) y = x == y


    replace _  = Has 

 
    unwrap (Has a) = Just a
    unwrap _ = Nothing

instance Container (Present t) where
    isEmpty (EmptyPresent _)= True
    isEmpty _= False

   
    containe (PresentFor _ x) y = x==y
    containe _ _ = False

    replace (EmptyPresent t) x  = PresentFor t x
    replace (PresentFor t _) y = PresentFor t y


    unwrap (PresentFor _ x) = Just x
    unwrap _ = Nothing





data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving Show

instance Container (MailedBox t d) where
    isEmpty (EmptyMailBox _ _) = True
    isEmpty _ = False

 
    containe (MailBoxTo _ _ x) y = x==y
    containe _ _ = False

    replace (EmptyMailBox t d )x = MailBoxTo t d x
    replace (MailBoxTo t d _) y = MailBoxTo t d y

    unwrap (MailBoxTo _ _ x) = Just x
    unwrap _ = Nothing





-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show,Ord,Eq)

data Experience = Programming | Managing | Leading deriving (Show,Ord,Eq)

type Address = String

data Salary = USD Double | EUR Double deriving (Show,Eq)

instance Ord Salary where
    compare (USD x ) (USD y)= x `compare` y
    compare (EUR x) (EUR y) = x `compare` y
    compare  (USD x)  (EUR  y) = x `compare` (1.1 *y)
    compare  (EUR x)  (USD y) = (x*1.1) `compare` y


data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address deriving (Show ,Eq,Ord)

compareRelationshipSalary:: Relationship->Relationship->Ordering
compareRelationshipSalary (Contractor _ _ sal1 _) (Contractor _ _ sal2 _) = compare sal1 sal2
compareRelationshipSalary (Employee _ _ sal1 _) (Employee _ _ sal2 _) = compare sal1 sal2
compareRelationshipSalary (Employee _ _ sal1 _) (Contractor _ _ sal2 _) = compare sal1 sal2
compareRelationshipSalary (Contractor _ _ sal1 _) (Employee _ _ sal2 _) = compare sal1 sal2


data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Show)

compareGen :: Pokemon->Pokemon->Ordering
compareGen pk1 pk2 = compare (pGeneration pk1) (pGeneration pk2)

comparePok :: Pokemon->Pokemon->Ordering
comparePok pk1 pk2 = compare (pPokeDexNum pk1) (pPokeDexNum pk2)

charizard = Pokemon "Charizard" ["Fire", "Flying"] 4 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 2 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

--Cach 1

-- -- Team memeber experience in years
-- newtype Exp = Exp Double deriving (Show,Num)

-- -- Team memeber data
-- type TeamMember = (String, Exp)

-- -- List of memeber of the team
-- team :: [TeamMember]
-- team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- -- Function to check the combined experience of the team
-- -- This function applied to `team` using GHCi should work
-- combineExp :: [TeamMember] -> Exp
-- combineExp = foldr ((+) . snd) 0

--Cach2

newtype Exp = Exp Double deriving (Show)
instance Num Exp where
  (+) (Exp a) (Exp b) = Exp (a+b)
  (-) (Exp a) (Exp b) = Exp (a-b)
  (*) (Exp a) (Exp b) = Exp (a*b)
  negate (Exp a)=Exp (negate a)
  abs (Exp a) = Exp (abs a)

  fromInteger a = Exp (fromInteger a)

  signum (Exp a) = Exp (signum a)



-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0 