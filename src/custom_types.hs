type Age = Int
type Height = Int
type LastName = String
type FirstName = String
type MiddleName = String
type PatientName = (String, String)

data RhType = Pos | Neg
data Sex = Male | Female
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Gabriel" "Jesus"
name2 = NameWithMiddle "Oscar" "D" "Grouch"

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh
-- >>> showBloodType (BloodType AB Pos) => "AB+"

canDonate :: BloodType -> BloodType -> Bool
canDonate (BloodType O _) _ = True -- O can donate to anybody
canDonate _ (BloodType AB _) = True -- AB can receive from anybody
canDonate (BloodType A _) (BloodType A _) = True -- A can donate to A
canDonate (BloodType B _) (BloodType B _) = True -- B can donate to B
canDonate _ _ = False --otherwise

-- Example
-- >>> canDonate patient1BT patient2BT => False
-- >>> canDonate patient2BT patient1BT => True
-- >>> canDonate patient2BT patient3BT => True
-- >>> canDonate patient1BT patient3BT => True
-- >>> canDonate patient3BT patient1BT => False

johnDoe :: Patient
johnDoe = Patient {
  name = Name "John" "Doe",
  age = 43,
  sex = Male,
  height = 74,
  weight = 200,
  bloodType = BloodType O Neg
}

-- Example
-- >>> height johnDoe => 74
-- >>> showBloodType (bloodType johnDoe) => "O-"
-- >>> showName (name johnDoe) => "John Doe"
