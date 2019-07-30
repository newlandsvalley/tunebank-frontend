module TuneBank.Api.Codec.Pagination
  ( PageNum
  , Pagination
  , defaultPagination
  , decodePagination
  , decodeJsonPageNum) where


import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, hush)
import Data.Maybe (fromMaybe)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodePoints (string, char, regex, skipSpaces)
import Data.Int (fromString)


-- | a page number in results from a search request
type PageNum =
  { page :: String
  , size :: String
  }

-- | decode the JSON results of a search request page number
decodeJsonPageNum :: Json -> Either String PageNum
decodeJsonPageNum json = do
   obj <- decodeJson json
   page <- obj .: "page"
   size <- obj .: "size"
   pure $ { page, size }

type Pagination =
  { page :: Int
  , maxPages :: Int
  }

defaultPagination :: Pagination
defaultPagination =
  { page : 1
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
  { page, maxPages }
