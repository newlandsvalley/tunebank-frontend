module TuneBank.Api.Codec.SearchPredicate
  ( SearchTerm
  , SearchPredicate
  , parsePredicate
  , printPredicate
  , encodePredicate) where

import Prelude ((<>), ($), (<$>), (<*>), (*>), (<<<), map, show)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (regex, string)
import Text.Parsing.StringParser.Combinators (sepBy)
import Data.Foldable (intercalate)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Bifunctor (lmap)
import Data.Nullable as Nullable


import Debug.Trace (spy, trace)

-- | search term
type SearchTerm =
  { key :: String
  , value :: String
  }

-- | Search Predicate over all terms
type SearchPredicate =
  Array SearchTerm

foreign import encodeURIComponent :: String -> String

foreign import decodeURIComponentImpl :: String -> Nullable.Nullable String

decodeURIComponent :: String -> Maybe String
decodeURIComponent = Nullable.toMaybe <<< decodeURIComponentImpl


printPredicate :: SearchPredicate -> String
printPredicate pred =
  intercalate "&" $ map printTerm pred


parsePredicate :: String -> Either String SearchPredicate
parsePredicate s =
  lmap show $ runParser text s

printTerm :: SearchTerm -> String
printTerm t =
  t.key <> "=" <> t.value

key :: Parser String
key =
  regex "[A-Za-z]*"

value :: Parser String
value =
  regex "[A-Za-z0-9 _,!\"'+-]*"

term :: Parser SearchTerm
term =
  buildTerm <$>
    key <*> (string "=" *> value)

predicate :: Parser SearchPredicate
predicate =
  fromFoldable <$>
    sepBy term (string "&")

text :: Parser SearchPredicate
text =
   predicate -- <* eof



-- Not sure if we need to use this because I don't know yet where URI encoding happens

encodePredicate :: SearchPredicate -> String
encodePredicate pred =
  let
    encoded = (encodeURIComponent <<< printPredicate) pred
    _ = spy "URI encoded pred" encoded
  in
    encoded

{-
encodePredicate :: SearchPredicate -> String
encodePredicate =
  encodeURIComponent <<< printPredicate
-}

buildTerm :: String -> String -> SearchTerm
buildTerm key value =
  { key, value }
