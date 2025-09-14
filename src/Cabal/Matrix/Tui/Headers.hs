module Cabal.Matrix.Tui.Headers
  ( HeaderState(..)
  , mkHeaderState
  , HeaderEditorState
  , initHeaderEditorState
  , headerEditorWidget
  , headerEditorHandleEvent
  , headerEditorKeybinds
  ) where

import Cabal.Matrix.Rectangle (Rectangle)
import Cabal.Matrix.Rectangle qualified as Rectangle
import Cabal.Matrix.Tui.Common
import Data.Foldable
import Data.Primitive
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty


data HeaderState = HeaderState
  { vertical :: Array Bool
    -- ^ INVARIANT: must have the same length as the number of columns in the
    -- build matrix in use
  , horizontalHeader :: Rectangle () Text (Maybe Text)
  , verticalHeader :: Rectangle () Text (Maybe Text)
  , gridToFlavor :: Rectangle () () (Maybe Int)
  }

mkHeaderState :: Rectangle flavor Text (Maybe Text) -> Array Bool -> HeaderState
mkHeaderState matrix vertical = HeaderState{..}
  where
    (horizontalHeader, verticalHeader, gridToFlavor)
      = Rectangle.unCartesianProduct vertical matrix

data HeaderEditorState = HeaderEditorState
  { index :: Int
  , scroll :: Int
  }

initHeaderEditorState :: HeaderEditorState
initHeaderEditorState = HeaderEditorState
  { index = 0
  , scroll = 0
  }

headerEditorWidget
  :: DisplayRegion
  -> Rectangle flavor Text (Maybe Text)
  -> HeaderState
  -> HeaderEditorState
  -> (HeaderEditorState, Image)
headerEditorWidget (_, height) matrix hs hes =
  ( hes { scroll = scroll' }
  , vertCat $ padToCommonWidth
    [ ( attr
      , ' '
      , text' (attr `withForeColor` yellow)
        (if indexArray hs.vertical i then "[V] " else "[H] ")
        <|> text' attr column
      )
      | (i, column) <- take height $ drop scroll'
        $ zip [0..] $ toList $ Rectangle.columns matrix
      , let
          attr = if i == hes.index
            then defAttr `withBackColor` blue
            else defAttr
    ]
  )
  where
    -- make it so that 1 <= hes.index - hes.scroll < height - 1
    scroll' = clamp (hes.index - height + 2) (hes.index - 1) hes.scroll

headerEditorHandleEvent
  :: Rectangle flavor Text (Maybe Text)
  -> Event
  -> (HeaderEditorState, HeaderState)
  -> (HeaderEditorState, HeaderState)
headerEditorHandleEvent matrix ev (hes, hs) = case ev of
  EvKey KUp _ ->
    ( hes { index = clamp 0 (sizeofArray hs.vertical - 1) $ pred hes.index }
    , hs
    )
  EvKey KDown _ ->
    ( hes { index = clamp 0 (sizeofArray hs.vertical - 1) $ succ hes.index }
    , hs
    )
  EvKey (isToggleKey -> True) _ | hes.index < sizeofArray hs.vertical
    -> (hes, mkHeaderState matrix $ toggleArray hes.index hs.vertical)
  _ -> (hes, hs)
  where
    isToggleKey = \case
      KChar ' ' -> True
      KEnter -> True
      _ -> False
    toggleArray i arr = runArray do
      m <- thawArray arr 0 (sizeofArray arr)
      writeArray m i . not =<< readArray m i
      pure m

headerEditorKeybinds :: [(Text, Text)]
headerEditorKeybinds =
  [ (Text.pack [triangleN, triangleS], "select field")
  , ("<Enter>/<Space>", "toggle vertical/horizontal axis")
  ]
