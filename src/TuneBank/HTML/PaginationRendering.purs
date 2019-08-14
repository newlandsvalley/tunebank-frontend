module TuneBank.HTML.PaginationRendering
  (renderPagination) where

import Prelude ((>), (<), (&&), (+), (-), (/), (<>), ($), (==), map, max, min, show)
import Data.Monoid (guard)
import Halogen.HTML as HH
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Route (Route)
import Data.Array (range)

-- failing attempt at abstracting the rendering of pagination

maxPageLinks :: Int
maxPageLinks = 10

renderPagination :: ∀ i p. Route -> Pagination ->  HH.HTML i p
renderPagination route pagination =
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
      , safeHref route
      ]
      html
    ]
