- Whenever you find yourself having to write a wrapper function around a maybe type:
  ```haskell
    lookupCredits :: UserName -> Maybe PlayerCredits
    lookupCredits username = Map.lookup username creditsDB

    -- wrapper function (Bad)
    altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
    altLookupCredits Nothing = Nothing
    altLookupCredits (Just username) = lookupCredits username
  ``
  This is a sure telling sign that there's a better alternative using `Functors`, `Applicatives` or `Chain`.