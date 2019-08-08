module TuneBank.Data.CommentId
  ( CommentId(..)
  , commentIdFromString
  , commentIdToString
  , fromNow
  ) where


import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))

newtype CommentId = CommentId String

derive newtype instance eqCommentId :: Eq CommentId
derive newtype instance ordCommentId :: Ord CommentId


instance showCommentid :: Show CommentId where
  show = commentIdToString

commentIdToString :: CommentId -> String
commentIdToString (CommentId s) =
  s

commentIdFromString :: String -> Either String CommentId
commentIdFromString s =
  Right $ CommentId s

fromNow :: Effect CommentId
fromNow = do
  instant <- now
  let
    (Milliseconds milliseconds) = unInstant instant
  pure $ CommentId (show milliseconds)
