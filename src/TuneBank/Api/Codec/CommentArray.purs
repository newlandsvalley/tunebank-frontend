module TuneBank.Api.Codec.CommentArray
  ( CommentArray
  , Comment
  , decodeComments) where


import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, decodeJson, encodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)

type Comment =
  { user :: String
  , cid :: String
  , subject :: String
  , text :: String
  }

decodeJsonComment :: Json -> Either String Comment
decodeJsonComment json = do
    obj <- decodeJson json
    user <- obj .: "user"
    cid <- obj .: "cid"
    subject <- obj .: "subject"
    text <- obj .: "text"
    pure $ { user, cid, subject, text }

type CommentArray = Array Comment

decodeCommentArray :: Json -> Either String CommentArray
decodeCommentArray json = decodeJson json >>= traverse decodeJsonComment

decodeComments :: Json -> Either String CommentArray
decodeComments json = do
  obj <- decodeJson json
  commentArray <- obj .: "comment" >>= decodeCommentArray
  pure commentArray
