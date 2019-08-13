module TuneBank.Navigation.SearchParams
  ( SearchParams
  , defaultSearchParams
  , parseParams )  where

-- | search parameters in common to the Router and the Endpoint

import Prelude ((<$>), (<*>), (*>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Either (Either)
import Data.Foldable (foldl)
import Text.Parsing.StringParser (Parser, ParseError, runParser)
import Text.Parsing.StringParser.Combinators (sepBy)
import Text.Parsing.StringParser.CodePoints (string, regex)

type SearchParams =
  { key :: Maybe String
  , rhythm :: Maybe String
  , title :: Maybe String
  , source :: Maybe String
  , transcriber :: Maybe String
  , abc :: Maybe String
  , page :: Int
  , sort :: String }

defaultSearchParams :: SearchParams
defaultSearchParams =
  { key: Nothing
  , rhythm: Nothing
  , title : Nothing
  , transcriber : Nothing
  , source : Nothing
  , abc : Nothing
  , page: 1
  , sort: "alpha" }

-- pare search parameters presented as a simple String
parseParams :: String -> Either ParseError SearchParams
parseParams =
  runParser searchParams

searchParams :: Parser SearchParams
searchParams =
  buildParams <$> sepBy keyValPair (string "&")

keyValPair :: Parser (Tuple String String)
keyValPair =
  Tuple <$> field <*> ((string "=") *> field)

field :: Parser String
field =
  regex "[^\\s\\r\\n=&]+"

-- warning - this is not particularky type-safe
buildParams :: List (Tuple String String) -> SearchParams
buildParams kvs =
  let
    f :: SearchParams -> Tuple String String -> SearchParams
    f params (Tuple k v ) =
      case k of
        "key" ->
          params { key = Just v }
        "rhythm" ->
          params { rhythm = Just v }
        "title" ->
          params { title = Just v }
        "transcriber" ->
          params { transcriber = Just v }
        "source" ->
          params { source = Just v }
        "abc" ->
          params { abc = Just v }
        _ ->
          params
  in
    foldl f defaultSearchParams kvs
