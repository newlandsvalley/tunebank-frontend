module TuneBank.Api.Codec.Comments
  ( Comments
  , Comment
  , Submission
  , defaultSubmission
  , decodeComments
  , encodeFormData) where


import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Traversable (traverse)

-- | the type of a comment when returned from the server
type Comment =
  { user :: String
  , cid :: String
  , subject :: String
  , text :: String
  }

-- | the type of a comment when submitted to the sever
type Submission =
  { user :: String
  , timestamp :: String
  , subject :: String
  , text :: String
  }

defaultSubmission :: Submission
defaultSubmission =
  { user : ""
  , timestamp : ""
  , subject : ""
  , text : ""
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

encodeFormData :: Submission -> FormURLEncoded
encodeFormData submission =
  fromArray
     [ Tuple "user"  (Just submission.user)
     , Tuple "timestamp"  (Just submission.timestamp)
     , Tuple "subject"  (Just submission.subject)
     , Tuple "text"  (Just submission.text)
     ]
