module TuneBank.Api.Codec.UsersPage
  ( UsersPage
  , UserRefArray
  , UserRef
  , decodeUsersPage) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (Pagination, decodeJsonPagination)

type UserRef =
  { name :: String
  , email :: String
  , valid :: String
  }

decodeJsonUserRef :: Json -> Either String UserRef
decodeJsonUserRef json = do
    obj <- decodeJson json
    name <- obj .: "name"
    email <- obj .: "email"
    valid <- obj .: "valid"
    pure $ { name, email, valid }

type UserRefArray = Array UserRef

type UsersPage =
  { users :: UserRefArray
  , pagination :: Pagination
  }

decodeUserRefArray :: Json -> Either String UserRefArray
decodeUserRefArray json = decodeJson json >>= traverse decodeJsonUserRef

decodeUsersPage :: Json -> Either String UsersPage
decodeUsersPage json = do
  obj <- decodeJson json
  users <- obj .: "user" >>= decodeUserRefArray
  pagination <- obj .: "pagination" >>= decodeJsonPagination
  pure $ { users, pagination }
