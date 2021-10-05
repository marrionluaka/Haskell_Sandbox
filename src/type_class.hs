class Describable a where
  describe :: a -> String

data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord)

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)
