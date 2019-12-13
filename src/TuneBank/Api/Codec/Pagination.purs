module TuneBank.Api.Codec.Pagination
  ( Pagination
  , defaultPagination
  , decodePagination
  , decodeJsonPagination) where


import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, hush)
import Data.Maybe (fromMaybe)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (string, char, regex, skipSpaces)
import Data.Int (fromString)

type Pagination =
  { page :: Int
  , size :: Int
  , maxPages :: Int
  }

-- | decode the JSON results of a search request page number
decodeJsonPagination :: Json -> Either String Pagination
decodeJsonPagination json = do
   obj <- decodeJson json
   pageS <- obj .: "page"
   sizeS <- obj .: "size"
   maxPagesS <- obj .: "maxPages"
   let
     page = (fromMaybe 1) $ fromString pageS
     size = (fromMaybe 15) $ fromString sizeS
     maxPages = (fromMaybe 1) $ fromString maxPagesS
   pure $ { page, size, maxPages }

defaultPagination :: Pagination
defaultPagination =
  { page : 1
  , size : 15
  , maxPages : 1
  }

decodePagination :: String -> Pagination
decodePagination s =
  fromMaybe defaultPagination $ hush $ runParser pagination s

pagination :: Parser Pagination
pagination =
  leftBracket *> proportion <* rightBracket

proportion :: Parser Pagination
proportion =
  buildPagination <$>
    anyInt <*> (ofText *> anyInt)

leftBracket :: Parser Char
leftBracket =
  char '['

rightBracket :: Parser Char
rightBracket =
  char ']'

ofText :: Parser String
ofText =
  skipSpaces *> string "of" <* skipSpaces

anyInt :: Parser Int
anyInt =
  fromMaybe 1 <$> fromString <$>
    regex "([1-9][0-9]*)"

buildPagination :: Int -> Int -> Pagination
buildPagination page maxPages =
  { page, size: 15, maxPages }
