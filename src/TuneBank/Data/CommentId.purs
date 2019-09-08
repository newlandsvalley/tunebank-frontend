module TuneBank.Data.CommentId
  ( CommentId(..)
  , CommentKey
  , commentIdFromString
  , commentIdToString
  , commentKey
  , fromNow
  ) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Number.Format (toString)

newtype CommentId = CommentId String

derive newtype instance eqCommentId :: Eq CommentId
derive newtype instance ordCommentId :: Ord CommentId


instance showCommentid :: Show CommentId where
  show = commentIdToString

-- the key to a comment is the combinatio  of the user who originally submitted the
-- | comment and the commentId (a timestamp)
type CommentKey =
  { user :: String
  , commentId :: CommentId
  }

commentIdToString :: CommentId -> String
commentIdToString (CommentId s) =
  s

commentIdFromString :: String -> Either String CommentId
commentIdFromString s =
  Right $ CommentId s

commentKey :: String -> CommentId -> CommentKey
commentKey user commentId =
  { user, commentId }

fromNow :: Effect CommentId
fromNow = do
  instant <- now
  let
    (Milliseconds milliseconds) = unInstant instant
  pure $ CommentId (toString milliseconds)
