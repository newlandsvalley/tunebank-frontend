module TuneBank.HTML.PaginationRendering
  (renderPagination) where

import Prelude ((>), (<), (&&), (+), (-), (/), (<>), ($), (==), map, max, min, show)
import Data.Monoid (guard)
import Halogen.HTML as HH
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Route (Route(..))
import Data.Array (range)

-- | abstraction for the rendering of pagination
-- | At the moment, only the user list and tune list are paged

maxPageLinks :: Int
maxPageLinks = 10

renderPagination :: ∀ i p. Route -> Pagination ->  HH.HTML i p
renderPagination route pagination =
  if (pagination.maxPages > 1) then
    renderPaginationLinks route pagination
  else
    HH.text ""

renderPaginationLinks :: ∀ i p. Route -> Pagination ->  HH.HTML i p
renderPaginationLinks route pagination =
  HH.ul
        [ css "pagination"]
        ( [ renderFirstPage  ] <>
          [ renderPrevPage  ] <>
            renderNumberedPageLinks  <>
          [ renderNextPage  ] <>
          [ renderLastPage  ]
        )
  where

    renderFirstPage  =
      if (pagination.maxPages > maxPageLinks  && pagination.page > 1) then
        paginationItem route 1 pagination.page
          [ HH.text "first" ]
      else
        HH.text ""

    renderLastPage  =
      if (pagination.maxPages > maxPageLinks  && pagination.page < pagination.maxPages) then
        paginationItem route pagination.maxPages pagination.page
          [ HH.text "last" ]
        else
          HH.text ""

    renderPrevPage  =
      if (pagination.page > 1) then
        paginationItem route (pagination.page -1) pagination.page
          [ HH.text "prev" ]
      else
        HH.text ""

    renderNextPage  =
      if (pagination.page < pagination.maxPages) then
        paginationItem route (pagination.page + 1) pagination.page
          [ HH.text "next" ]
      else
        HH.text ""

    renderNumberedPageLinks  =
      let
        pageLink n =
          paginationItem route n pagination.page
            [ HH.text (show n) ]
        first =
          max 1 (pagination.page - (maxPageLinks / 2))
        last =
          min pagination.maxPages (first + maxPageLinks)
      in
        map pageLink (range first last)


paginationItem
  :: ∀ i p.
  Route ->
  Int ->
  Int ->
  Array (HH.HTML i p) ->
  HH.HTML i p
paginationItem route thisPage currentPage html =
  HH.li
    [ css "pagination-item" ]
    [ HH.a
      [ css $ guard (thisPage == currentPage) "current"
      , safeHref $ pagedRoute thisPage route
      ]
      html
    ]

-- | Produce a route for the correct page
-- | we need a case entry for each tunebank 'page;' which is itself paged
pagedRoute :: Int -> Route -> Route
pagedRoute thisPage r =
  case r of
    UserList pageParams ->
      UserList pageParams { page = thisPage }
    TuneList searchParams ->
      TuneList searchParams { page = thisPage }
    _ ->
      r
