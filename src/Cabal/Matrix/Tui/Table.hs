module Cabal.Matrix.Tui.Table
  ( TableMeta(..)
  , TableHeaders(..)
  , TableContents(..)
  , tableLayout
  , TableLayout
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
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Primitive.Array
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty


data TableMeta = TableMeta
  { frozenRows :: Int
  , normalRows :: Int
  , frozenColumns :: Int
  , normalColumns :: Int
  }

data TableHeaders = TableHeaders
  { frozenRowHeader :: Int -> Text
  , frozenRowCell :: Int -> Int -> Maybe Text
  , frozenColumnHeader :: Int -> Text
  , frozenColumnCell :: Int -> Int -> Maybe Text
  }

newtype TableContents = TableContents
  { normalCell :: Int -> Int -> Maybe FlavorState
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

data TopHeaderLayout = TopHeaderLayout
  { columns :: [NonEmpty Text]
  , startY :: Int
  , endY :: Int
  }

data LeftHeaderLayout = LeftHeaderLayout
  { rows :: [Text]
  , startX :: Int
  , endX :: Int
  }

data TableLayout = TableLayout
  { top :: Array TopHeaderLayout
  , topLeft :: Image
  , left :: Array LeftHeaderLayout
  , leftTop :: Image
  }

tableLayout :: TableMeta -> TableHeaders -> TableLayout
tableLayout meta headers = TableLayout
  { top = arrayFromListN meta.frozenRows top
  , topLeft = vertCat topLeft
  , left = arrayFromListN meta.frozenColumns left
  , leftTop = horizCat $ intersperse (char defAttr ' ') leftTop
  }
  where
    (topLeft, top) = unzip $ snd $ mapAccumL mkTop 0
      [ (rowHeader', columns', height)
      | row <- [0 .. meta.frozenRows - 1]
      , let
          rowHeader = text' defAttr $ headers.frozenRowHeader row
          columns =
            [ foldMap (wrap matrixCellWidth) $ headers.frozenRowCell col row
            | col <- [0 .. meta.normalColumns - 1]
            ]
          height = maximum (imageHeight rowHeader :| (length <$> columns))
          rowHeader' = resizeHeight height rowHeader
          columns' =
            [ fmap (padTextToWidth matrixCellWidth)
              $ fromMaybe (NonEmpty.singleton "") $ NonEmpty.nonEmpty
              $ column <> replicate (height - length columns) ""
            | column <- columns
            ]
      ]
    mkTop !startY (rowHeader, columns, height)
      | !endY <- startY + height
      = (endY, (rowHeader, TopHeaderLayout { columns, startY, endY }))


    (leftTop, left) = unzip $ snd $ mapAccumL mkLeft 0
      [ (columnHeader', rows', width)
      | col <- [0 .. meta.frozenColumns - 1]
      , let
          columnHeader = text' defAttr $ headers.frozenColumnHeader col
          rows =
            [ fromMaybe "" $ headers.frozenColumnCell col row
            | row <- [0 .. meta.normalRows - 1]
            ]
          width = maximum (imageWidth columnHeader :| (wctwidth <$> rows))
          columnHeader' = resizeWidth width columnHeader
          rows' = padTextToWidth width <$> rows
      ]
    mkLeft !startX (columnHeader, rows, width)
      | !endX <- startX + width
      , !nextStartX <- endX + 1
      = (nextStartX, (columnHeader, LeftHeaderLayout {rows, startX, endX}))

padTextToWidth :: Int -> Text -> Text
padTextToWidth !width !t = t <> Text.replicate (width - Text.length t) " "

tableWidget
  :: DisplayRegion
  -> TableMeta
  -> TableContents
  -> TableLayout
  -> TableState
  -> (TableState, Image)
tableWidget (outputWidth, outputHeight) meta table layout state =
  ( state { frozenScrollX, frozenScrollY, normalScrollX, normalScrollY }
  , image
  )
  where
    top = vertCat
      [ horizCat $ intersperse (char defAttr ' ')
        [ vertCat $ text' attr <$> NonEmpty.toList column
        | (icol, column) <- zip [0..] row.columns
        , let
            attr = if state.normalSelectionCol == icol
              && state.frozenSelectionRow == irow
              && state.activeSelection == SelectionTop
              then defAttr `withBackColor` blue
              else defAttr
        ]
      | (irow, row) <- zip [0..] $ toList layout.top
      ]
    left = horizCat $ intersperse (char defAttr ' ')
      [ vertCat
        [ text' attr row
        | (irow, row) <- zip [0..] col.rows
        , let
            attr = if state.frozenSelectionCol == icol
              && state.normalSelectionRow == irow
              && state.activeSelection == SelectionLeft
              then defAttr `withBackColor` blue
              else defAttr
        ]
      | (icol, col) <- zip [0..] $ toList layout.left
      ]
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

    topLeftWidth = max (imageWidth layout.leftTop) (imageWidth layout.topLeft)

    frozenWidth = min topLeftWidth ((outputWidth - 1) `div` 2)
    normalWidth = outputWidth - 1 - frozenWidth
    frozenHeight = min (imageHeight layout.topLeft) ((outputHeight - 3) `div` 2)
    normalHeight = outputHeight - 3 - frozenHeight

    frozenScrollX
      | state.frozenSelectionCol < sizeofArray layout.left
      , LeftHeaderLayout{ startX, endX }
        <- indexArray layout.left state.frozenSelectionCol
      = clamp (endX - frozenWidth) startX state.frozenScrollX
      | otherwise = state.frozenScrollX
    frozenScrollY
      | state.frozenSelectionRow < sizeofArray layout.top
      , TopHeaderLayout{ startY, endY }
        <- indexArray layout.top state.frozenSelectionRow
      = clamp (endY - frozenHeight) startY state.frozenScrollY
      | otherwise = state.frozenScrollY
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
        (translateY (-frozenScrollY) layout.topLeft)
        <|> scrollBorderY canFrozenScrollN canFrozenScrollS frozenHeight
        <|> crop normalWidth frozenHeight
          (translate (-normalScrollX) (-frozenScrollY) top)
      , scrollBorderX canFrozenScrollW canFrozenScrollE frozenWidth
        <|> char defAttr borderNESW
        <|> scrollBorderX canNormalScrollW canNormalScrollE normalWidth
      , resizeWidth frozenWidth
        (translateX (-frozenScrollX) layout.leftTop)
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
