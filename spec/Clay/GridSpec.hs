{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Clay.GridSpec where

import Test.Hspec
import Common
import Clay
import Data.These (These (..))


spec :: Spec
spec = do
  describe "gap" $ do
    "{gap:10px;grid-gap:10px}"
      `shouldRenderFrom`
      gap (px 10)

  describe "rowGap" $ do
    "{row-gap:5px;grid-row-gap:5px}"
      `shouldRenderFrom`
      rowGap (px 5)

  describe "columnGap" $ do
    "{column-gap:1em;grid-column-gap:1em}"
      `shouldRenderFrom`
      columnGap (em 1)

  describe "gridTemplateRows" $ do
    describe "keyword" $ do
      "{grid-template-rows:none}"
        `shouldRenderFrom`
        gridTemplateRows none

    describe "list of sizes" $ do
      "{grid-template-rows:50px fit-content(500px) minmax(400em,50%)}"
        `shouldRenderFrom`
        gridTemplateRows [upcast (px 50), upcast $ fitContent (px 500), minmax (em 400) (pct 50)]

  describe "gridTemplateColumns" $ do
    describe "keyword" $ do
      "{grid-template-columns:none}"
        `shouldRenderFrom`
        gridTemplateColumns none

    describe "list of sizes" $ do
      "{grid-template-columns:1em calc(20% + 1fr) auto}"
        `shouldRenderFrom`
        gridTemplateColumns [upcast $ em 1, pct 20 @+@ fr 1, upcast $ auto]

  describe "gridAutoRows" $ do
    describe "keywords" $ do
      "{grid-auto-rows:min-content}"
        `shouldRenderFrom`
        gridAutoRows minContent

      "{grid-auto-rows:max-content}"
        `shouldRenderFrom`
        gridAutoRows maxContent

    describe "list of sizes" $ do
      "{grid-auto-rows:1em calc(20% + 1fr) auto}"
        `shouldRenderFrom`
        gridAutoRows [upcast $ em 1, pct 20 @+@ fr 1, upcast $ auto]

  describe "gridAutoFlow" $ do
    "{grid-auto-flow:row}"
      `shouldRenderFrom`
      gridAutoFlow row

    "{grid-auto-flow:column}"
      `shouldRenderFrom`
      gridAutoFlow column

    "{grid-auto-flow:dense}"
      `shouldRenderFrom`
      gridAutoFlow dense

    "{grid-auto-flow:row dense}"
      `shouldRenderFrom`
      gridAutoFlow rowDense

    "{grid-auto-flow:column dense}"
      `shouldRenderFrom`
      gridAutoFlow columnDense

  describe "gridArea" $ do
    "{grid-area:header}"
      `shouldRenderFrom`
      gridArea "header"

  describe "grid coordinate properties" $ do

    "{grid-row-start:3}"
      `shouldRenderFrom`
      gridRowStart 3

    "{grid-row-end:-2}"
      `shouldRenderFrom`
      gridRowEnd (-2)

    "{grid-column-start:span nav}"
      `shouldRenderFrom`
      gridColumnStart $ gridLocation Span (That "nav")

    "{grid-column-end:3 footer}"
      `shouldRenderFrom`
      gridColumnEnd $ gridLocation NoSpan (These 3 "footer")


  describe "gridTemplateAreas" $ do
    describe "keyword values" $ do
      "{grid-template-areas:none}"
        `shouldRenderFrom`
        gridTemplateAreas none

      "{grid-template-areas:inherit}"
        `shouldRenderFrom`
        gridTemplateAreas inherit

      "{grid-template-areas:initial}"
        `shouldRenderFrom`
        gridTemplateAreas initial

      "{grid-template-areas:unset}"
        `shouldRenderFrom`
        gridTemplateAreas unset

    describe "mozilla example" $ do
      let
        area_a = "a"
        area_b = "b"
        area_c = "c"
        area_blank = blankGridArea

      "{grid-template-areas:\"a a .\"\n\"a a .\"\n\". b c\"}"
        `shouldRenderFrom`
        gridTemplateAreas
          [ [ area_a, area_a, area_blank]
          , [ area_a, area_a, area_blank]
          , [ area_blank, area_b, area_c]
          ]
    describe "non rectangular template areas should error" $ do
      let
        area_a = "a"
        area_b = "b"
        area_c = "c"
        area_blank = blankGridArea

      GridTemplateNamedAreas_NotRectangular
        `shouldErrorFromRender`
        gridTemplateAreas
          [ [ area_blank]                 -- length 1
          , [ area_a, area_blank]         -- length 2
          , [ area_blank, area_b, area_c] -- length 3
          ]

    describe "empty template should error" $ do
      GridTemplateNamedAreas_Empty
        `shouldErrorFromRender`
        gridTemplateAreas []

    describe "template with empty row(s) should error" $ do
      GridTemplateNamedAreas_EmptyRow
        `shouldErrorFromRender`
        gridTemplateAreas [[], []]
