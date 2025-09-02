module Cabal.Matrix.Tui.Common where

import Data.Foldable
import Data.Functor
import Data.Text (Text)
import Graphics.Vty
import Text.Wrap


borderNS :: Char
borderNS = '\x2502'

borderEW :: Char
borderEW = '\x2500'

borderNESW :: Char
borderNESW = '\x253C'

triangleN :: Char
triangleN = '\x25B2'

triangleE :: Char
triangleE = '\x25B6'

triangleS :: Char
triangleS = '\x25BC'

triangleW :: Char
triangleW = '\x25C0'

wrap :: Int -> Text -> [Text]
wrap = wrapTextToLines defaultWrapSettings { breakLongWords = True }

resizeWidthFill :: Attr -> Char -> Int -> Image -> Image
resizeWidthFill attr ch width image
  = case compare (imageWidth image) width of
    GT -> cropRight width image
    EQ -> image
    LT -> image
      <|> charFill attr ch (width - imageWidth image) (imageHeight image)

resizeHeightFill :: Attr -> Char -> Int -> Image -> Image
resizeHeightFill attr ch height image
  = case compare (imageHeight image) height of
    GT -> cropBottom height image
    EQ -> image
    LT -> image
      <-> charFill attr ch (imageWidth image) (height - imageHeight image)

padToCommonWidth :: (Foldable t, Functor t) => t (Attr, Char, Image) -> t Image
padToCommonWidth xs
  = xs <&> \(attr, ch, image) -> resizeWidthFill attr ch width image
  where
    width = maximum $ 0 : toList (xs <&> \(_, _, image) -> imageWidth image)

padToCommonHeight :: (Foldable t, Functor t) => t (Attr, Char, Image) -> t Image
padToCommonHeight xs
  = xs <&> \(attr, ch, image) -> resizeHeightFill attr ch height image
  where
    height = maximum $ 0 : toList (xs <&> \(_, _, image) -> imageHeight image)

clamp :: Ord a => a -> a -> a -> a
clamp lower upper = max lower . min upper
