newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show


data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDaryFarmer _ = False

data FarmerRec = FarmerRec { name       :: Name
                           , acres      :: Acres
                           , farmerType :: FarmerType }
                           deriving Show


isDairyFarmer :: FarmerRec -> Bool
isDairyFarmer farmer = case farmerType farmer of
                         DairyFarmer -> True
                         _           -> False


-- DO NOT DO THIS
-- data Automobile = Null | Car { make :: String ...}

-- When possible, separate out product type that uses records into own type

data Car = Car { make :: String
               , model ::  String
               , year :: Integer }
               deriving Show

-- Still not great, but better!
-- Avoid using Null - use Maybe instead
data Automobile = Null | Automobile Car deriving Show
