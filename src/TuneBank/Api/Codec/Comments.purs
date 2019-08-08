module TuneBank.Api.Codec.Comments
  ( Comments
  , Comment
  , defaultComment
  , decodeComments
  , encodeFormData) where


import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Data.CommentId (CommentId(..))

-- | the type of a comment when returned from the server
type Comment =
  { user :: String
  , commentId :: CommentId   -- Id is a unique timestamp (cid on database)
  , subject :: String
  , text :: String
  }


defaultComment :: Comment
defaultComment =
  { user : ""
  , commentId : CommentId ""
  , subject : ""
  , text : ""
  }

decodeJsonComment :: Json -> Either String Comment
decodeJsonComment json = do
    obj <- decodeJson json
    user <- obj .: "user"
    timestamp <- obj .: "cid"
    subject <- obj .: "subject"
    text <- obj .: "text"
    pure $ { user, commentId : CommentId timestamp, subject, text }

type Comments = Array Comment

decodeCommentArray :: Json -> Either String Comments
decodeCommentArray json = decodeJson json >>= traverse decodeJsonComment

decodeComments :: Json -> Either String Comments
decodeComments json = do
  obj <- decodeJson json
  comments <- obj .: "comment" >>= decodeCommentArray
  pure comments

encodeFormData :: Comment -> FormURLEncoded
encodeFormData comment =
  let
    (CommentId timestamp) = comment.commentId
  in
    fromArray
       [ Tuple "user"  (Just comment.user)
       , Tuple "timestamp"  (Just timestamp)
       , Tuple "subject"  (Just comment.subject)
       , Tuple "text"  (Just comment.text)
       ]
