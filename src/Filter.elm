module Filter exposing (..)
import Tuple exposing (first, second)

type Filter = Filter String Category

type Category
  = All
  | Unfinished
  | Finished


new : String -> Category -> Filter
new q c =
  Filter q c

query : Filter -> String
query = unpack >> first

category : Filter -> Category
category = unpack >> second

unpack : Filter -> (String, Category)
unpack (Filter q c) =
  (q, c)

mapQuery : (String -> String) -> Filter -> Filter
mapQuery f (Filter q c) =
  Filter (f q) c

mapCategory : (Category -> Category) -> Filter -> Filter
mapCategory f (Filter q c) =
  Filter q (f c)