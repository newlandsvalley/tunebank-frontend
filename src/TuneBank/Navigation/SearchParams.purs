module TuneBank.Navigation.SearchParams
  (SearchParams, defaultSearchParams)  where

-- | search parameters in common to the Router and the Endpoint

import Data.Maybe (Maybe(..))

type SearchParams =
  { key :: Maybe String
  , rhythm :: Maybe String
  , title :: Maybe String
  , source :: Maybe String
  , transcriber :: Maybe String
  , abc :: Maybe String
  , page :: Int
  , sort :: String }

defaultSearchParams :: SearchParams
defaultSearchParams =
  { key: Nothing
  , rhythm: Nothing
  , title : Nothing
  , transcriber : Nothing
  , source : Nothing
  , abc : Nothing
  , page: 1
  , sort: "alpha" }
