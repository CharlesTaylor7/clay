{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , TypeFamilies
  , RankNTypes
  , ExistentialQuantification
  , PatternSynonyms
  , DataKinds
  , TypeOperators
  , ConstraintKinds
  , FlexibleContexts
  , UndecidableInstances
  , QuantifiedConstraints
  , FlexibleInstances
  #-}
-- | Implementation of <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout>.
module Clay.Grid
  ( gap
  , rowGap
  , columnGap
  , gridTemplateRows
  , gridTemplateColumns
  , GridTrackList
  , gridAutoRows
  , gridAutoColumns
  , GridAutoTrackList
  , gridAutoFlow
  , row
  , column
  , dense
  , rowDense
  , columnDense
  , gridArea
  , blankGridArea
  , GridArea
  , gridRowStart
  , gridRowEnd
  , gridColumnStart
  , gridColumnEnd
  , GridLocation
  , gridLocation
  , IsSpan(..)
  , gridTemplateAreas
  , GridTemplateAreas
  -- re exports
  , These(..)
  )
  where

import Clay.Common
import Clay.Property
import Clay.Size
import Clay.Stylesheet
import Clay.Elements (span)

import Prelude hiding (span)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce (coerce)
import Data.These (These(..))
import Data.IndexedListLiterals (IndexedListLiterals)
import qualified Data.IndexedListLiterals as Sized
import GHC.Exts (IsList(..))
import GHC.TypeLits (KnownNat, CmpNat)


-- | Property sets the gaps (gutters) between rows and columns.
-- Sets both "gap" & "grid-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/gap
-- "CSS Grid Layout initially defined the grid-gap property. This prefixed property is being replaced by gap. However, in order to support browsers that implemented grid-gap and not gap for grid, you will need to use the prefixed property"
gap :: Size a -> Css
gap = key "gap" <> key "grid-gap"

-- | Property sets the size of the gap (gutter) between an element's grid rows.
-- Sets both "row-gap" & "grid-row-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap
-- "CSS Grid Layout initially defined the grid-row-gap property. This prefixed property is being replaced by row-gap. However, in order to support browsers that implemented grid-row-gap and not row-gap for grid, you will need to use the prefixed property."
rowGap :: Size a -> Css
rowGap = key "row-gap" <> key "grid-row-gap"

-- | Property sets the size of the gap (gutter) between an element's grid columns.
-- Sets both "column-gap" & "grid-column-gap"
-- to quote: https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap
-- "CSS Grid Layout initially defined the grid-column-gap property. This prefixed property is being replaced by column-gap. However, in order to support bcolumnsers that implemented grid-column-gap and not column-gap for grid, you will need to use the prefixed property."
columnGap :: Size a -> Css
columnGap = key "column-gap" <> key "grid-column-gap"

-------------------------------------------------------------------------------

-- | Property defines the line names and track sizing functions of the grid rows.
gridTemplateRows :: GridTrackList a -> Css
gridTemplateRows = key "grid-template-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridTemplateColumns :: GridTrackList a -> Css
gridTemplateColumns = key "grid-template-columns"

newtype GridTrackList a = GridTrackList Value
  deriving (Val, None, Inherit, Initial, Unset)

instance IsList (GridTrackList a) where
  type Item (GridTrackList a) = Size a
  toList = error ""
  fromList = GridTrackList . noCommas

-------------------------------------------------------------------------------

-- | Property defines the line names and track sizing functions of the grid rows.
gridAutoRows :: GridAutoTrackList a -> Css
gridAutoRows = key "grid-auto-rows"

-- | Property defines the line names and track sizing functions of the grid columns.
gridAutoColumns :: GridAutoTrackList a -> Css
gridAutoColumns = key "grid-auto-columns"

newtype GridAutoTrackList a = GridAutoTrackList Value
  deriving (Val, Auto, MinContent, MaxContent, Inherit, Initial, Unset)

instance IsList (GridAutoTrackList a) where
  type Item (GridAutoTrackList a) = Size a
  toList = error ""
  fromList = GridAutoTrackList . noCommas

-------------------------------------------------------------------------------
gridAutoFlow :: GridAutoFlow -> Css
gridAutoFlow = key "grid-auto-flow"

newtype GridAutoFlow = GridAutoFlow Value
  deriving (Val, Row, Column, Inherit, Initial, Unset)

dense :: GridAutoFlow
dense = GridAutoFlow "dense"

rowDense :: GridAutoFlow
rowDense = GridAutoFlow "row dense"

columnDense :: GridAutoFlow
columnDense = GridAutoFlow "column dense"
-------------------------------------------------------------------------------
-- | Property defines the element location inside grid template
gridArea :: GridArea -> Css
gridArea = key "grid-area"

blankGridArea :: GridArea
blankGridArea = GridArea "."

newtype GridArea = GridArea Text
  deriving (IsString, Val)

-------------------------------------------------------------------------------

gridRowStart :: GridLocation -> Css
gridRowStart = key "grid-row-start"

gridRowEnd :: GridLocation -> Css
gridRowEnd = key "grid-row-end"

gridColumnStart :: GridLocation -> Css
gridColumnStart = key "grid-column-start"

gridColumnEnd :: GridLocation -> Css
gridColumnEnd = key "grid-column-end"

gridLocation :: IsSpan -> These Integer GridArea -> GridLocation
gridLocation isSpan = GridLocation_Data . GridLocationData isSpan

data IsSpan = Span | NoSpan
  deriving (Show, Eq)

data GridLocation
  = GridLocation_Keyword Value
  | GridLocation_Data GridLocationData

instance Val GridLocation where
  value (GridLocation_Keyword v) = v
  value (GridLocation_Data d) = value d

instance Auto    GridLocation where auto    = GridLocation_Keyword auto
instance Inherit GridLocation where inherit = GridLocation_Keyword inherit
instance Initial GridLocation where initial = GridLocation_Keyword initial
instance Unset   GridLocation where unset   = GridLocation_Keyword unset

-- See under syntax:
-- https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start
-- either grid index and/or named grid area is required, but span is optional
data GridLocationData = GridLocationData IsSpan (These Integer GridArea)

instance Val GridLocationData where
  value (GridLocationData isSpan indexAndOrGridArea) =
    if isSpan == Span
    then value (span :: Value, indexAndOrGridArea)
    else value indexAndOrGridArea

pattern GridIndex :: Integer -> GridLocation
pattern GridIndex n = GridLocation_Data (GridLocationData NoSpan (This n))

instance Num GridLocation where
  -- for index literals
  fromInteger = GridIndex
  -- for negative index literals
  negate (GridIndex index) = GridIndex $ negate index
  -- in general we don't support arithmetic on this type
  negate _ = error "negate not defined for GridLocation"
  abs = error "abs not defined for GridLocation"
  signum = error "abs not defined for GridLocation"
  (+) = error "addition not defined for GridLocation"
  (*) = error "multiplication not defined for GridLocation"

-------------------------------------------------------------------------------

-- | Property defines the template for grid layout
gridTemplateAreas :: IsGridTemplateAreas areas => areas -> Css
gridTemplateAreas = key "grid-template-areas"

newtype KeywordGridTemplateAreas = KeywordGridTemplateAreas Value
  deriving (Val, None, Inherit, Initial, Unset)

class Val a => IsGridTemplateAreas a
-- | Any keyword value allowed for grid-template-areas.
instance IsGridTemplateAreas KeywordGridTemplateAreas
-- | Compile time checked Grid template areas. A single row as a tuple.
instance forall n v. StaticGridTemplateRow n v => IsGridTemplateAreas v
-- | Compile time checked Grid template areas. Accepts a tuple of tuples
instance forall m n v w. StaticGridTemplateAreas m n v w => IsGridTemplateAreas v
-- | Run time checked Grid template areas. A list of lists
instance IsGridTemplateAreas DynamicGridTemplateAreas

instance {-# OVERLAPS #-} forall m n v w. StaticGridTemplateAreas m n v w => Val v where
  value = gridTemplateAreasToValue . map Sized.toList . Sized.toList

instance {-# OVERLAPS #-} forall n v. StaticOneRowGridTemplateAreas n v => Val v where
  value = gridTemplateAreasToValue . pure . Sized.toList

type PositiveNat n = (KnownNat n, CmpNat n 0 ~ 'GT)

type StaticRowGridTemplateArea numCols inputRow =
  ( PositiveNat numCols
  , IndexedListLiterals inputRow numCols GridArea
  )

type StaticGridTemplateAreas numCols numRows inputRows inputRow =
  ( PositiveNat numCols
  , PositiveNat numRows
  , IndexedListLiterals inputRow numCols GridArea
  , IndexedListLiterals inputRows numRows inputRow
  )

gridTemplateAreasToValue :: [[GridArea]] -> Value
gridTemplateAreasToValue areas =
    let
      rows = coerce areas :: [[Text]]
      wrapInParens text = "\"" <> text <> "\""
      convertRow = wrapInParens . Text.intercalate " "
    in
      value $
      Text.intercalate "\n" $
      fmap convertRow $
      rows

