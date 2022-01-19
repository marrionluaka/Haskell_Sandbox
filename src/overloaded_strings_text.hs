{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

-- ++ cannot be used with T.Text so instead we use Monoids or Semigroups

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"


myLines:: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n"
