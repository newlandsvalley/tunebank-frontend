module TuneBank.Api.Codec.Comments
  ( Comments
  , Comment
  , cleanComment
  , defaultComment
  , decodeComment
  , decodeComments
  , encodeFormData) where


import Prelude

import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either)
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Maybe (Maybe(..))
import Data.String (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import TuneBank.Data.CommentId (CommentId(..))

-- | the type of a comment when returned from the server
type Comment =
  { user :: String
  , commentId :: CommentId   -- Id is a unique timestamp (cid on database)
  , subject :: String
  , text :: String
  }

-- | remove any unwanted text which may invalidate any JSON representation
cleanComment :: Comment -> Comment
cleanComment comment =
  let 
    subject = cleanText comment.subject
    text = cleanText comment.text
  in 
    comment { subject = subject 
            , text = text
            }
  where 
  -- | replace any double quotes with single quotes
  cleanText :: String -> String
  cleanText =
     replaceAll (Pattern "\"") (Replacement "'")

defaultComment :: Comment
defaultComment =
  { user : ""
  , commentId : CommentId ""
  , subject : ""
  , text : ""
  }

-- | decode a JSON comment
decodeComment :: Json -> Either JsonDecodeError Comment
decodeComment json = do
    obj <-  decodeJson json
    user <- obj .: "user"
    timestamp <- obj .: "cid"
    subject <- obj .: "subject"
    text <- obj .: "text"
    pure $ { user, commentId : CommentId timestamp, subject, text }



type Comments = Array Comment

decodeCommentArray :: Json -> Either JsonDecodeError Comments
decodeCommentArray json = decodeJson json >>= traverse decodeComment

decodeComments :: Json -> Either JsonDecodeError Comments
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
