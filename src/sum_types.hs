type LastName = String
type FirstName = String
type MiddleName = String

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLastName Char Char LastName

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

-- Usage
hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLastName 'H' 'P' "Lovecraft"))



--------------
data Book = Book {
  author      :: Creator,
  isbn        :: String,
  bookTitle   :: String,
  bookYear    :: Int,
  bookPrice   :: Double
}

data VinylRecord = VinylRecord {
  artist        :: Creator,
  recordTitle   :: String,
  recordYear    :: Int,
  recordPrice   :: Double
}

data CollectibleToy = CollectibleToy {
  name          :: String,
  description   :: String,
  toyPrice      :: Double
}

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy

-- Usage
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
