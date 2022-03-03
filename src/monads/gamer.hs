import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1, "nYarlathoTep")
                          ,(2, "KINGinYELLOW")
                          ,(3, "dagon1997")
                          ,(4, "rcarter1919")
                          ,(5, "xCTHULHUx")
                          ,(6, "yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep", 2000)
                         ,("KINGinYELLOW", 15000)
                         ,("dagon1997", 300)
                         ,("rcarter1919", 12)
                         ,("xCTHULHUx", 50000)
                         ,("yogSOThoth", 150000)]


-- calculations
-- creditFromId :: GamerId -> Maybe PlayerCredits
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB


