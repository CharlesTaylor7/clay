{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Partial implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
module Clay.Grid
  ( gap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , GridTemplateSizes
  --, SomeSize(..)
  , gridTemplateAreas
  , gridArea
  , blankGridArea
  , GridArea(..)
  , GridTemplateAreas
  , GridTemplateNamedAreas
  , mkGridTemplateNamedAreas
  , unGridTemplateNamedAreas
  , InvalidGridTemplateNamedAreas(..)
  -- deprecated
  , gridGap
  )
  where

import Clay.Common
import Clay.Property
import Clay.Size
import Clay.Stylesheet

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Coerce (coerce)
import GHC.Exts (IsList(..))
import Control.Exception (Exception(..), throw)
import Control.Monad (when)


-- | Property sets the gaps (gutters) between rows and columns.
gap :: Size a -> Css
gap = key "gap" <> key "grid-gap"

gridGap :: Size a -> Css
gridGap = gap
{-# DEPRECATED gridGap "Use gap, rowGap, and/or columnGap instead" #-}

-- | Property sets the size of the gap (gutter) between an element's grid rows.
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the size of the gap (gutter) between an element's grid columns.
columnGap :: Size a -> Css
columnGap = key "column-gap" <> key "grid-column-gap"

-- | Property defines the line names and track sizing functions of the grid rows.
gridTemplateRows :: GridTemplateSizes -> Css
gridTemplateRows = key "grid-template-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: GridTemplateSizes -> Css
gridTemplateColumns = key "grid-template-columns"

-- | Property defines the template for grid layout
gridTemplateAreas :: GridTemplateAreas -> Css
gridTemplateAreas = key "grid-template-areas"

-- | Property defines the element location inside grid template
gridArea :: GridArea -> Css
gridArea = key "grid-area"

newtype GridArea = GridArea Text
  deriving (IsString, Val)

blankGridArea :: GridArea
blankGridArea = GridArea "."

-------------------------------------------------------------------------------
newtype GridTemplateSizes = GridTemplateSizes Value
  deriving (Val, None, Inherit, Initial, Unset)

instance IsList GridTemplateSizes where
  type Item GridTemplateSizes = Size AnyUnit
  toList = error ""
  fromList = GridTemplateSizes . noCommas

-------------------------------------------------------------------------------
newtype GridTemplateAreas = GridTemplateAreas Value
  deriving (Val, None, Inherit, Initial, Unset)

instance IsList GridTemplateAreas where
  type Item GridTemplateAreas = [GridArea]
  toList = error "toList GridTemplateAreas is not defined"
  fromList = GridTemplateAreas . value . fromList'
    where
      fromList' :: [Item GridTemplateNamedAreas] -> GridTemplateNamedAreas
      fromList' = fromList

-- have to create a newtype to override the Val instance for lists
newtype GridTemplateNamedAreas = GridTemplateNamedAreas { unGridTemplateNamedAreas :: [[GridArea]] }

instance Val GridTemplateNamedAreas where
  value areas =
    let
      rows = coerce areas :: [[Text]]
      wrapInParens text = "\"" <> text <> "\""
      convertRow = wrapInParens . Text.intercalate " "
    in
      value $
      Text.intercalate "\n" $
      map convertRow $
      rows

-- | toList will throw when your grid template areas are invalid
instance IsList GridTemplateNamedAreas where
  type Item GridTemplateNamedAreas = [GridArea]
  toList = unGridTemplateNamedAreas . coerce
  fromList = fromRightOrThrow . mkGridTemplateNamedAreas
    where
      fromRightOrThrow :: Exception e => Either e a -> a
      fromRightOrThrow (Right a) = a
      fromRightOrThrow (Left e) = throw e

-- | Smart constructor for GridTemplateNamedAreas
mkGridTemplateNamedAreas :: [[GridArea]] -> Either InvalidGridTemplateNamedAreas GridTemplateNamedAreas
mkGridTemplateNamedAreas rows = do
    let
      counts = map length (coerce rows :: [[GridArea]])
      longest = maximum counts

    when (null rows ) $
      Left GridTemplateNamedAreas_Empty

    when (any (== 0) counts)  $
      Left GridTemplateNamedAreas_EmptyRow

    when (any (/= longest) counts)  $
      Left GridTemplateNamedAreas_NotRectangular

    Right $ GridTemplateNamedAreas rows

-- | Failure modes for the smart constructor
data InvalidGridTemplateNamedAreas
  = GridTemplateNamedAreas_Empty
  | GridTemplateNamedAreas_EmptyRow
  | GridTemplateNamedAreas_NotRectangular
  deriving (Eq, Show)

instance Exception InvalidGridTemplateNamedAreas
------------------------------------------------------------------------------
