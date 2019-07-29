module TuneBank.Api.Codec.Comments
  ( Comments
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

type Comments = Array Comment

decodeCommentArray :: Json -> Either String Comments
decodeCommentArray json = decodeJson json >>= traverse decodeJsonComment

decodeComments :: Json -> Either String Comments
decodeComments json = do
  obj <- decodeJson json
  comments <- obj .: "comment" >>= decodeCommentArray
  pure comments
