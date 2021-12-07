import Data.List

names = [("Karim", "Benzema"),
  ("Robert", "Lewandosky"),
  ("Kylian", "Mbappe"),
  ("Erling", "Haaland")]

compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 > lastName2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
