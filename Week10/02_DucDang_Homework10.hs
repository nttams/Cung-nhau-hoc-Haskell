{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add ayou  function called `unwrap` that gives back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

-- Create a type class Container
class Container c where
  isEmpty :: c a -> Bool
  contain :: (Eq a) => c a -> a -> Bool
  replace :: c a -> b -> c b
  unwrap :: c a -> Maybe a

-- Create an instance for the MailedBox data type
instance Container (MailedBox t d) where
  isEmpty (EmptyMailBox t d) = True
  isEmpty _ = False
  contain (MailBoxTo t d a) b = a==b
  contain (EmptyMailBox t d) _ = False
  replace (MailBoxTo t d _) b = MailBoxTo t d b
  replace (EmptyMailBox t d) b = MailBoxTo t d b
  unwrap (MailBoxTo _ _ a) = Just a
  unwrap _ = Nothing

data Box a = Empty | Has a
data Present t a = EmptyPresentFor t | PresentFor t a

-- Create instances for Box
instance Container Box where
  isEmpty Empty = True
  isEmpty _ = False
  contain (Has a) b = a==b
  contain Empty _ = False
  replace _ b = Has b  
  unwrap (Has a) = Just a
  unwrap _ = Nothing

-- Create instances for Present
instance Container (Present t) where
  isEmpty (EmptyPresentFor t) = True
  isEmpty _ = False
  contain (PresentFor t a) b = a==b
  contain (EmptyPresentFor _) _ = False
  replace (PresentFor t _) b = PresentFor t b
  replace (EmptyPresentFor t) b = PresentFor t b
  unwrap (PresentFor _ a) = Just a
  unwrap _ = Nothing

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double 

-- Create instances of Eq type class for Salary
instance Eq Salary where
  (==) (USD a) (USD b) = a == b
  (==) (EUR a) (EUR b) = a == b
  (==) (USD a) (EUR b) = 0.93 *a == b 
  (==) (EUR a) (USD b) = a == b * 0.93 

-- Create instances of Ord for Salary
instance Ord Salary where
  (USD a) `compare` (USD b) = a `compare` b
  (EUR a) `compare` (EUR b) = a `compare` b
  (USD a) `compare` (EUR b) = (0.93 * a) `compare` b
  (EUR a) `compare` (USD b) = a `compare` (0.93 * b)
  (USD a) <= (USD b)= a <= b
  (EUR a) <= (EUR b) = a <= b
  (USD a) <= (EUR b) = (0.93 * a ) <= b
  (EUR a) <= (USD b) = a <= (0.93 * b)

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address 

instance Eq Relationship where
  (==) (Employee _ _ (USD a) _) (Employee _ _ (USD b) _) = a == b
  (==) (Employee _ _ (EUR a) _) (Employee _ _ (EUR b) _) = a == b
  (==) (Employee _ _ (USD a) _) (Employee _ _ (EUR b) _) = 0.93 * a == b
  (==) (Employee _ _ (EUR a) _) (Employee _ _ (USD b) _) = a == b * 0.93
  (==) (Employee _ _ (USD a) _) (Contractor _ _ (USD b) _) = a == b
  (==) (Employee _ _ (EUR a) _) (Contractor _ _ (EUR b) _) = a == b
  (==) (Employee _ _ (USD a) _) (Contractor _ _ (EUR b) _) = 0.93 * a == b 
  (==) (Employee _ _ (EUR a) _) (Contractor _ _ (USD b) _) = a == b * 0.93

  (==) (Contractor _ _ (USD a) _) (Contractor _ _ (USD b) _) = a == b
  (==) (Contractor _ _ (EUR a) _) (Contractor _ _ (EUR b) _) = a == b
  (==) (Contractor _ _ (USD a) _) (Contractor _ _ (EUR b) _) = 0.93 * a == b
  (==) (Contractor _ _ (EUR a) _) (Contractor _ _ (USD b) _) = a == b * 0.93
  (==) (Contractor _ _ (USD a) _) (Employee _ _ (USD b) _) = a == b
  (==) (Contractor _ _ (EUR a) _) (Employee _ _ (EUR b) _) = a == b
  (==) (Contractor _ _ (USD a) _) (Employee _ _ (EUR b) _) = 0.93 * a == b 
  (==) (Contractor _ _ (EUR a) _) (Employee _ _ (USD b) _) = a == b * 0.93

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Show) -- add 'deriving Show'

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Num)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
