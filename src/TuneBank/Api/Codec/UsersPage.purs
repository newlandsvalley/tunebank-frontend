module TuneBank.Api.Codec.UsersPage
  ( UsersPage
  , UserRefArray
  , UserRef
  , decodeUsersPage) where


import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, encodeJson, decodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (PageNum, decodeJsonPageNum)

type UserRef =
  { name :: String
  , email :: String
  }

decodeJsonUserRef :: Json -> Either String UserRef
decodeJsonUserRef json = do
    obj <- decodeJson json
    name <- obj .: "name"
    email <- obj .: "email"
    pure $ { name, email }


type UserRefArray = Array UserRef

type UsersPage =
  { users :: UserRefArray
  , pageNum :: PageNum
  }

decodeUserRefArray :: Json -> Either String UserRefArray
decodeUserRefArray json = decodeJson json >>= traverse decodeJsonUserRef

decodeUsersPage :: Json -> Either String UsersPage
decodeUsersPage json = do
  obj <- decodeJson json
  users <- obj .: "user" >>= decodeUserRefArray
  pageNum <- obj .: "pagination" >>= decodeJsonPageNum
  pure $ { users, pageNum }
