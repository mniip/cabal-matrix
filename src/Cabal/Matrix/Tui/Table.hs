module Cabal.Matrix.Tui.Table
  ( TableMeta(..)
  , Table(..)
  , TableState(..)
  , ActiveSelection(..)
  , initTableState
  , tableWidget
  , tableHandleEvent
  , tableKeybinds
  ) where

import Cabal.Matrix.Tui.Common
import Cabal.Matrix.Tui.Flavor
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty


data TableMeta = TableMeta
  { frozenRows :: Int
  , normalRows :: Int
  , frozenColumns :: Int
  , normalColumns :: Int
  }

data Table = Table
  { frozenRowHeader :: Int -> Text
  , frozenRowCell :: Int -> Int -> Maybe Text
  , frozenColumnHeader :: Int -> Text
  , frozenColumnCell :: Int -> Int -> Maybe Text
  , normalCell :: Int -> Int -> Maybe FlavorState
  }

data ActiveSelection
  = SelectionLeft
  | SelectionTop
  | SelectionNormal
  | SelectionNone
  deriving stock (Eq)

data TableState = TableState
  { frozenScrollX :: Int
  , frozenScrollY :: Int
  , normalScrollX :: Int
  , normalScrollY :: Int
  , frozenSelectionCol :: Int
  , frozenSelectionRow :: Int
  , normalSelectionCol :: Int
  , normalSelectionRow :: Int
  , activeSelection :: ActiveSelection
  }

initTableState :: TableState
initTableState = TableState
  { frozenScrollX = 0
  , frozenScrollY = 0
  , normalScrollX = 0
  , normalScrollY = 0
  , frozenSelectionCol = 0
  , frozenSelectionRow = 0
  , normalSelectionCol = 0
  , normalSelectionRow = 0
  , activeSelection = SelectionNone
  }

tableWidget
  :: DisplayRegion -> TableMeta -> Table -> TableState -> (TableState, Image)
tableWidget (outputWidth, outputHeight) meta table state =
  ( state { frozenScrollX, frozenScrollY, normalScrollX, normalScrollY }
  , image
  )
  where
    (topLeftParts, topParts) = unzip
      [ (rowHeader, horizCat rowCells)
      | row <- [0 .. meta.frozenRows - 1]
      , let
          rowHeader :| rowCells = padToCommonHeight $
            ( defAttr
            , ' '
            , text' defAttr $ table.frozenRowHeader row
            ) :|
            intersperse (defAttr, ' ', char defAttr ' ')
            [ ( attr
              , ' '
              , resizeWidthFill attr ' ' matrixCellWidth
                if imageHeight txtImage == 0
                then char attr ' '
                else txtImage
              )
            | col <- [0 .. meta.normalColumns - 1]
            , let
                attr = if state.frozenSelectionRow == row
                  && state.normalSelectionCol == col
                  && state.activeSelection == SelectionTop
                  then defAttr `withBackColor` blue
                  else defAttr
                txtImage = vertCat
                  [ text' attr line
                  | line <- foldMap (wrap matrixCellWidth)
                    $ table.frozenRowCell col row
                  ]
            ]
      ]
    topLeft = vertCat topLeftParts -- TODO: this would look better right-aligned
    top = vertCat topParts
    topHeights = imageHeight <$> topParts
    (leftTopParts, leftParts) = unzip $
      intersperse (char defAttr ' ', charFill defAttr ' ' 1 meta.normalRows)
      [ (columnHeader, vertCat columnCells)
      | col <- [0 .. meta.frozenColumns - 1]
      , let
          columnHeader :| columnCells = padToCommonWidth $
            ( defAttr
            , ' '
            , text' defAttr $ table.frozenColumnHeader col
            ) :|
            [ ( attr
              , ' '
              , resizeHeightFill attr ' ' matrixCellHeight
                $ text' attr $ fold $ table.frozenColumnCell col row
              )
            | row <- [0 .. meta.normalRows - 1]
            , let
                attr = if state.frozenSelectionCol == col
                  && state.normalSelectionRow == row
                  && state.activeSelection == SelectionLeft
                  then defAttr `withBackColor` blue
                  else defAttr
            ]
      ]
    leftTop = horizCat leftTopParts
    left = horizCat leftParts
    leftWidths = imageWidth <$> leftParts
    normal = vertCat
      [ horizCat $
        intersperse (char defAttr ' ')
        [ cellWidget
          (state.normalSelectionCol == col
            && state.normalSelectionRow == row
            && state.activeSelection == SelectionNormal)
          $ table.normalCell col row
        | col <- [0 .. meta.normalColumns - 1]
        ]
      | row <- [0 .. meta.normalRows - 1]
      ]

    topLeftWidth = max (sum leftWidths) (imageWidth topLeft)

    frozenWidth = min topLeftWidth ((outputWidth - 1) `div` 2)
    normalWidth = outputWidth - 1 - frozenWidth
    frozenHeight = min (sum topHeights) ((outputHeight - 3) `div` 2)
    normalHeight = outputHeight - 3 - frozenHeight

    frozenScrollX = case extentAt leftWidths (state.frozenSelectionCol * 2) of
      (l, r) -> clamp (r - frozenWidth) l state.frozenScrollX
    frozenScrollY = case extentAt topHeights state.frozenSelectionRow of
      (l, r) -> clamp (r - frozenHeight) l state.frozenScrollY
    normalScrollX = clamp
      ((state.normalSelectionCol + 1) * (matrixCellWidth + 1) - 1 - normalWidth)
      (state.normalSelectionCol * (matrixCellWidth + 1))
      state.normalScrollX
    normalScrollY = clamp
      ((state.normalSelectionRow + 1) * matrixCellHeight - normalHeight)
      (state.normalSelectionRow * matrixCellHeight)
      state.normalScrollY

    canFrozenScrollW = frozenScrollX > 0
    canFrozenScrollE = frozenScrollX < topLeftWidth - frozenWidth
    canFrozenScrollN = frozenScrollY > 0
    canFrozenScrollS = frozenScrollY < imageHeight top - frozenHeight
    canNormalScrollW = normalScrollX > 0
    canNormalScrollE = normalScrollX < imageWidth normal - normalWidth
    canNormalScrollN = normalScrollY > 0
    canNormalScrollS = normalScrollY < imageHeight normal - normalHeight

    image = vertCat
      [ resize frozenWidth frozenHeight
        (translateY (-frozenScrollY) topLeft)
        <|> scrollBorderY canFrozenScrollN canFrozenScrollS frozenHeight
        <|> crop normalWidth frozenHeight
          (translate (-normalScrollX) (-frozenScrollY) top)
      , scrollBorderX canFrozenScrollW canFrozenScrollE frozenWidth
        <|> char defAttr borderNESW
        <|> scrollBorderX canNormalScrollW canNormalScrollE normalWidth
      , resizeWidth frozenWidth
        (translateX (-frozenScrollX) leftTop)
        <|> char defAttr borderNS
      , scrollBorderX canFrozenScrollW canFrozenScrollE frozenWidth
        <|> char defAttr borderNESW
        <|> scrollBorderX canNormalScrollW canNormalScrollE normalWidth
      , resize frozenWidth normalHeight
        (translate (-frozenScrollX) (-normalScrollY) left)
        <|> scrollBorderY canNormalScrollN canNormalScrollS normalHeight
        <|> crop normalWidth normalHeight
          (translate (-normalScrollX) (-normalScrollY) normal)
      ]

extentAt :: [Int] -> Int -> (Int, Int)
extentAt = go 0
  where
    go !acc (!x:_) 0 = (acc, acc + x)
    go acc (x:xs) n = go (acc + x) xs (n - 1)
    go _ [] _ = (0, 0)

scrollBorderX :: Bool -> Bool -> Int -> Image
scrollBorderX left right width
  | width > 2 = horizCat
    [ char defAttr (if left then triangleW else borderEW)
    , charFill defAttr borderEW (width - 2) 1
    , char defAttr (if right then triangleE else borderEW)
    ]
  | otherwise = charFill defAttr borderEW width 1

scrollBorderY :: Bool -> Bool -> Int -> Image
scrollBorderY up down height
  | height > 2 = vertCat
    [ char defAttr (if up then triangleN else borderNS)
    , charFill defAttr borderNS 1 (height - 2)
    , char defAttr (if down then triangleS else borderNS)
    ]
  | otherwise = charFill defAttr borderNS 1 height

tableHandleEvent :: TableMeta -> Event -> TableState -> TableState
tableHandleEvent meta ev ts = case ev of
  EvKey (KChar '\t') _ -> ts
    { activeSelection = case ts.activeSelection of
      SelectionNone -> SelectionNormal
      SelectionNormal -> SelectionTop
      SelectionTop -> SelectionLeft
      SelectionLeft -> SelectionNone
    }
  EvKey KLeft _
    | ts.activeSelection `elem` [SelectionNormal, SelectionTop] -> ts
      { normalSelectionCol = clamp 0 (meta.normalColumns - 1)
        $ pred ts.normalSelectionCol
      }
    | ts.activeSelection == SelectionLeft -> ts
      { frozenSelectionCol = clamp 0 (meta.frozenColumns - 1)
        $ pred ts.frozenSelectionCol
      }
    | ts.activeSelection == SelectionNone
    -> ts { activeSelection = SelectionNormal }
  EvKey KRight _
    | ts.activeSelection `elem` [SelectionNormal, SelectionTop] -> ts
      { normalSelectionCol = clamp 0 (meta.normalColumns - 1)
        $ succ ts.normalSelectionCol
      }
    | ts.activeSelection == SelectionLeft -> ts
      { frozenSelectionCol = clamp 0 (meta.frozenColumns - 1)
        $ succ ts.frozenSelectionCol
      }
    | ts.activeSelection == SelectionNone
    -> ts { activeSelection = SelectionNormal }
  EvKey KUp _
    | ts.activeSelection `elem` [SelectionNormal, SelectionLeft] -> ts
      { normalSelectionRow = clamp 0 (meta.normalRows - 1)
        $ pred ts.normalSelectionRow
      }
    | ts.activeSelection == SelectionTop -> ts
      { frozenSelectionRow = clamp 0 (meta.frozenRows - 1)
        $ pred ts.frozenSelectionRow
      }
    | ts.activeSelection == SelectionNone
    -> ts { activeSelection = SelectionNormal }
  EvKey KDown _
    | ts.activeSelection `elem` [SelectionNormal, SelectionLeft] -> ts
      { normalSelectionRow = clamp 0 (meta.normalRows - 1)
        $ succ ts.normalSelectionRow
      }
    | ts.activeSelection == SelectionTop -> ts
      { frozenSelectionRow = clamp 0 (meta.frozenRows - 1)
        $ succ ts.frozenSelectionRow
      }
    | ts.activeSelection == SelectionNone
    -> ts { activeSelection = SelectionNormal }
  _ -> ts

tableKeybinds :: [(Text, Text)]
tableKeybinds =
  [ (Text.pack [triangleW, triangleN, triangleS, triangleE], "select cell")
  , ("<Tab>", "focus cells/cols/rows")
  ]
