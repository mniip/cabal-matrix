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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  { rowIndex :: Int
  , rowHeader :: Image
  , columns :: Array (NonEmpty Text)
  , height :: Int
  }

data LeftHeaderLayout = LeftHeaderLayout
  { columnIndex :: Int
  , columnHeader :: Image
  , rows :: Array Text
  , width :: Int
  }

data TableLayout = TableLayout
  { rowHeaderWidth :: Int
  , top :: Map Int TopHeaderLayout
  , topHeight :: Int
  , left :: Map Int LeftHeaderLayout
  , leftWidth :: Int
  }

tableLayout :: TableMeta -> TableHeaders -> TableLayout
tableLayout meta headers = TableLayout
  { rowHeaderWidth
  , top
  , topHeight
  , left
  , leftWidth = max 0 (pred leftWidth' - 1)
    -- ^ Remove the width of the trailing separator if there were any columns,
    -- but don't underflow to negative if there weren't.
  }
  where
    (topHeight, Map.fromAscList -> top) = mapAccumL mkTop 0
      [ TopHeaderLayout
        { rowIndex = row
        , rowHeader = resizeHeight height rowHeader
        , columns = arrayFromListN meta.normalColumns
          [ fmap (padTextToWidth matrixCellWidth)
            $ fromMaybe (NonEmpty.singleton "") $ NonEmpty.nonEmpty
            $ column <> replicate (height - length columns) ""
          | column <- columns
          ]
        , height
        }
      | row <- [0 .. meta.frozenRows - 1]
      , let
          rowHeader = text' defAttr $ headers.frozenRowHeader row
          columns =
            [ foldMap (wrap matrixCellWidth) $ headers.frozenRowCell col row
            | col <- [0 .. meta.normalColumns - 1]
            ]
          height = maximum (imageHeight rowHeader :| (length <$> columns))
      ]
    mkTop !startY !layout
      | !endY <- startY + layout.height = (endY, (endY, layout))

    rowHeaderWidth = maximum
      $ 0 :| (imageWidth . (.rowHeader) <$> Map.elems top)

    (leftWidth', Map.fromAscList -> left) = mapAccumL mkLeft 0
      [ LeftHeaderLayout
        { columnIndex = col
        , columnHeader = resizeWidth width columnHeader
        , rows = arrayFromListN meta.normalRows $ padTextToWidth width <$> rows
        , width
        }
      | col <- [0 .. meta.frozenColumns - 1]
      , let
          columnHeader = text' defAttr $ headers.frozenColumnHeader col
          rows =
            [ fromMaybe "" $ headers.frozenColumnCell col row
            | row <- [0 .. meta.normalRows - 1]
            ]
          width = maximum (imageWidth columnHeader :| (wctwidth <$> rows))
      ]
    mkLeft !startX layout
      | !endX <- startX + layout.width
      , !nextStartX <- endX + 1 = (nextStartX, (endX, layout))

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
    topLeftWidth = max layout.rowHeaderWidth layout.leftWidth

    frozenWidth = min topLeftWidth ((outputWidth - 1) `div` 2)
    normalWidth = outputWidth - 1 - frozenWidth
    frozenHeight = min layout.topHeight ((outputHeight - 3) `div` 2)
    normalHeight = outputHeight - 3 - frozenHeight

    frozenScrollX
      | state.frozenSelectionCol < Map.size layout.left
      , (endX, col) <- Map.elemAt state.frozenSelectionCol layout.left
      = clamp (endX - frozenWidth) (endX - col.width) state.frozenScrollX
      | otherwise = state.frozenScrollX
    frozenScrollY
      | state.frozenSelectionRow < Map.size layout.top
      , (endY, row) <- Map.elemAt state.frozenSelectionRow layout.top
      = clamp (endY - frozenHeight) (endY - row.height) state.frozenScrollY
      | otherwise = state.frozenScrollY
    normalScrollX = clamp
      ((state.normalSelectionCol + 1) * (matrixCellWidth + 1) - 1 - normalWidth)
      (state.normalSelectionCol * (matrixCellWidth + 1))
      state.normalScrollX
    normalScrollY = clamp
      ((state.normalSelectionRow + 1) * matrixCellHeight - normalHeight)
      (state.normalSelectionRow * matrixCellHeight)
      state.normalScrollY

    normalTransX = -(normalScrollX `mod` (matrixCellWidth + 1))
    normalStartCol = normalScrollX `div` (matrixCellWidth + 1)
    normalEndCol = min (meta.normalColumns - 1)
      $ -((-normalScrollX - normalWidth) `div` (matrixCellWidth + 1))
    normalTransY = -(normalScrollY `mod` matrixCellHeight)
    normalStartRow = normalScrollY `div` matrixCellHeight
    normalEndRow = min (meta.normalRows - 1)
      $ -((-normalScrollY - normalHeight) `div` matrixCellHeight)
    (frozenTransX, leftVisible) =
      case Map.dropWhileAntitone (<= frozenScrollX) layout.left of
        leftTail@(Map.lookupMin -> Just (firstEndX, firstCol)) ->
          ( firstEndX - firstCol.width - frozenScrollX
          , snd <$> takeWhile
            (\(endX, col) -> endX - col.width < frozenScrollX + frozenWidth)
            (Map.toList leftTail)
          )
        _ -> (0, [])
    (frozenTransY, topVisible) =
      case Map.dropWhileAntitone (<= frozenScrollY) layout.top of
        topTail@(Map.lookupMin -> Just (firstEndY, firstRow)) ->
          ( firstEndY - firstRow.height - frozenScrollY
          , snd <$> takeWhile
            (\(endY, row) -> endY - row.height < frozenScrollY + frozenHeight)
            (Map.toList topTail)
          )
        _ -> (0, [])

    topLeft = cropBottom frozenHeight
      $ translate (frozenWidth - layout.rowHeaderWidth) frozenTransY
      $ vertCat
      [row.rowHeader | row <- topVisible]
    top = crop normalWidth frozenHeight
      $ translate normalTransX frozenTransY
      $ vertCat
      [ horizCat $ intersperse (backgroundFill 1 1)
        [ vertCat $ text' attr <$> NonEmpty.toList (indexArray row.columns col)
        | col <- [normalStartCol..normalEndCol]
        , let
            attr = if state.normalSelectionCol == col
              && state.frozenSelectionRow == row.rowIndex
              && state.activeSelection == SelectionTop
              then defAttr `withBackColor` blue
              else defAttr
        ]
      | row <- topVisible
      ]
    leftTop = cropRight frozenWidth $ translateX frozenTransX
      $ horizCat $ intersperse (backgroundFill 1 1)
      [col.columnHeader | col <- leftVisible]
    left = crop frozenWidth normalHeight
      $ translate frozenTransX normalTransY
      $ horizCat $ intersperse (backgroundFill 1 1)
      [ vertCat
        [ text' attr $ indexArray col.rows row
        | row <- [normalStartRow..normalEndRow]
        , let
            attr = if state.frozenSelectionCol == col.columnIndex
              && state.normalSelectionRow == row
              && state.activeSelection == SelectionLeft
              then defAttr `withBackColor` blue
              else defAttr
        ]
      | col <- leftVisible
      ]
    normal = crop normalWidth normalHeight
      $ translate normalTransX normalTransY
      $ vertCat
      [ horizCat $ intersperse (backgroundFill 1 1)
        [ cellWidget
          (state.normalSelectionCol == col
            && state.normalSelectionRow == row
            && state.activeSelection == SelectionNormal)
          $ table.normalCell col row
        | col <- [normalStartCol..normalEndCol]
        ]
      | row <- [normalStartRow..normalEndRow]
      ]

    canFrozenScrollW = frozenScrollX > 0
    canFrozenScrollE = frozenScrollX < topLeftWidth - frozenWidth
    canFrozenScrollN = frozenScrollY > 0
    canFrozenScrollS = frozenScrollY < layout.topHeight - frozenHeight
    canNormalScrollW = normalScrollX > 0
    canNormalScrollE = normalScrollX <
      meta.normalColumns * (matrixCellWidth + 1) - 1 - normalWidth
    canNormalScrollN = normalScrollY > 0
    canNormalScrollS = normalScrollY <
      meta.normalRows * matrixCellHeight - normalHeight

    image = vertCat
      [ topLeft
        <|> scrollBorderY canFrozenScrollN canFrozenScrollS frozenHeight
        <|> top
      , scrollBorderX canFrozenScrollW canFrozenScrollE frozenWidth
        <|> char defAttr borderNESW
        <|> scrollBorderX canNormalScrollW canNormalScrollE normalWidth
      , leftTop
        <|> char defAttr borderNS
      , scrollBorderX canFrozenScrollW canFrozenScrollE frozenWidth
        <|> char defAttr borderNESW
        <|> scrollBorderX canNormalScrollW canNormalScrollE normalWidth
      , left
        <|> scrollBorderY canNormalScrollN canNormalScrollS normalHeight
        <|> normal
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
