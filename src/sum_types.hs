type LastName = String
type FirstName = String
type MiddleName = String

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLastName Char Char LastName

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLastName 'H' 'P' "Lovecraft"))
